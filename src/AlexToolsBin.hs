{-# LANGUAGE TemplateHaskell, CPP #-}
module AlexToolsBin
  ( -- * Lexer Basics
    initialInput, Input(..), inputFile
  , Lexeme(..)
  , SourcePos(..), startPos, beforeStartPos, prevPos
  , SourceRange(..)
  , prettySourcePos, prettySourceRange
  , prettySourcePosLong, prettySourceRangeLong
  , HasRange(..)
  , (<->)
  , moveSourcePos

    -- * Writing Lexer Actions
  , Action(..)

    -- ** Lexemes
  , lexeme
  , matchLength
  , matchRange
  , matchBytes

    -- ** Manipulating the lexer's state
  , getLexerState
  , setLexerState

    -- ** Access to the lexer's input
  , startInput
  , endInput

    -- * Interface with Alex
  , AlexInput
  , alexInputPrevChar
  , alexGetByte
  , makeLexer
  , LexerConfig(..)
  , simpleLexer
  , Word8

  ) where

import           Control.DeepSeq
import           Data.Word(Word8)
import           Data.ByteString(ByteString)
import qualified Data.ByteString as BS
import           Data.Text(Text)
import qualified Data.Text as Text
import           Control.Monad(liftM,ap,replicateM)
import           Language.Haskell.TH
#if !MIN_VERSION_base(4,8,0)
import           Control.Applicative
#endif

data Lexeme t = Lexeme
  { lexemeBytes :: !ByteString
  , lexemeToken :: !t
  , lexemeRange :: !SourceRange
  } deriving (Show, Eq)

instance NFData t => NFData (Lexeme t) where
  rnf (Lexeme x y z) = rnf (x,y,z)

data SourcePos = SourcePos
  { sourceIndex :: !Int
  , sourceFile  :: !Text
  } deriving (Show, Eq)

-- | Pretty print the source position without the file name.
prettySourcePos :: SourcePos -> String
prettySourcePos x = show (sourceIndex x)

-- | Pretty print the source position, including the file name.
prettySourcePosLong :: SourcePos -> String
prettySourcePosLong x = Text.unpack (sourceFile x) ++ ":" ++ show (sourceIndex x)

instance NFData SourcePos where
  rnf (SourcePos {}) = ()

-- | Update a 'SourcePos' for a particular matched character
moveSourcePos :: SourcePos -> SourcePos
moveSourcePos p = p { sourceIndex = sourceIndex p + 1 }

-- | A range in the source code.
data SourceRange = SourceRange
  { sourceFrom :: !SourcePos
  , sourceTo   :: !SourcePos
  } deriving (Show, Eq)

-- | Pretty print the range, without the file name
prettySourceRange :: SourceRange -> String
prettySourceRange x = prettySourcePos (sourceFrom x) ++ "--" ++
                      prettySourcePos (sourceTo x)

-- | Pretty print the range, including the file name.
prettySourceRangeLong :: SourceRange -> String
prettySourceRangeLong x
  | sourceFile pfrom == sourceFile pto =
    Text.unpack (sourceFile pfrom) ++ ":" ++
    prettySourcePos pfrom ++ "--" ++ prettySourcePos pto
  | otherwise = prettySourcePosLong pfrom ++ "--" ++
                prettySourcePosLong pto
  where
  pfrom = sourceFrom x
  pto   = sourceTo x



instance NFData SourceRange where
  rnf (SourceRange {}) = ()

class HasRange t where
  range :: t -> SourceRange

instance HasRange SourcePos where
  range p = SourceRange { sourceFrom = p, sourceTo = p }

instance HasRange SourceRange where
  range = id

instance HasRange (Lexeme t) where
  range = lexemeRange

instance (HasRange a, HasRange b) => HasRange (Either a b) where
  range (Left x)  = range x
  range (Right x) = range x

(<->) :: (HasRange a, HasRange b) => a -> b -> SourceRange
x <-> y = SourceRange { sourceFrom = sourceFrom (range x)
                      , sourceTo   = sourceTo   (range y)
                      }

--------------------------------------------------------------------------------

-- | An action to be taken when a regular expression matchers.
newtype Action s a = A { runA :: Input -> Input -> Int -> s -> (s, a) }

instance Functor (Action s) where
  fmap = liftM

instance Applicative (Action s) where
  pure a = A (\_ _ _ s -> (s,a))
  (<*>)  = ap

instance Monad (Action s) where
  return = pure
  A m >>= f = A (\i1 i2 l s -> let (s1,a)    = m i1 i2 l s
                                   A m1 =  f a
                               in m1 i1 i2 l s1)

-- | Acces the input just before the regular expression started matching.
startInput :: Action s Input
startInput = A (\i1 _ _ s -> (s,i1))

-- | Acces the input just after the regular expression that matched.
endInput :: Action s Input
endInput = A (\_ i2 _ s -> (s,i2))

-- | The number of characters in the matching input.
matchLength :: Action s Int
matchLength = A (\_ _ l s -> (s,l))

-- | Acces the curent state of the lexer.
getLexerState :: Action s s
getLexerState = A (\_ _ _ s -> (s,s))

-- | Change the state of the lexer.
setLexerState :: s -> Action s ()
setLexerState s = A (\_ _ _ _ -> (s,()))

-- | Get the range for the matching input.
matchRange :: Action s SourceRange
matchRange =
  do i1 <- startInput
     i2 <- endInput
     return (inputPos i1 <-> inputPrev i2)

-- | Get the text associated with the matched input.
matchBytes :: Action s ByteString
matchBytes =
  do i1 <- startInput
     n  <- matchLength
     return (BS.take n (inputBytes i1))

-- | Use the token and the current match to construct a lexeme.
lexeme :: t -> Action s [Lexeme t]
lexeme tok =
  do r   <- matchRange
     txt <- matchBytes
     let l = Lexeme { lexemeRange = r
                    , lexemeToken = tok
                    , lexemeBytes = txt
                    }
     l `seq` return [ l ]

-- | Information about the lexer's input.
data Input = Input
  { inputPos      :: {-# UNPACK #-} !SourcePos
    -- ^ Current input position.

  , inputBytes    :: {-# UNPACK #-} !ByteString
    -- ^ The text that needs to be lexed.

  , inputPrev     :: {-# UNPACK #-} !SourcePos
    -- ^ Location of the last consumed character.

  , inputPrevByte :: {-# UNPACK #-} !Word8
    -- ^ The last consumed character.
  }

-- | Prepare the text for lexing.
initialInput :: Text {- ^ Where the text came from -} ->
                ByteString {- ^ The text to lex -} -> Input
initialInput file str = Input
  { inputPos      = startPos file
  , inputPrev     = beforeStartPos file
  , inputPrevByte = 0
  , inputBytes    = str
  }

startPos :: Text {- ^ Name of file/thing containing this -} -> SourcePos
startPos file = SourcePos { sourceIndex = 0, sourceFile = file }

beforeStartPos :: Text -> SourcePos
beforeStartPos file = SourcePos { sourceIndex = -1, sourceFile = file }

{- | Move one position back.  Assumes that newlines use a single bytes.

This function is intended to be used when starting the lexing somewhere
in the middle of the input, for example, if we are implementing a quasi
quoter, and the previous part of the input is not in our language.
-}
prevPos :: SourcePos -> SourcePos
prevPos p
  | sourceIndex p < 0 = p
  | otherwise         = p { sourceIndex = sourceIndex p - 1 }

-- | The file/thing for the current position.
inputFile :: Input -> Text
inputFile = sourceFile . inputPos

--------------------------------------------------------------------------------
-- | Lexer configuration.
data LexerConfig s t = LexerConfig
  { lexerInitialState :: s
    -- ^ State that the lexer starts in

  , lexerStateMode :: s -> Int
    -- ^ Determine the current lexer mode from the lexer's state.

  , lexerEOF       :: s -> SourcePos -> [Lexeme t]
    -- ^ Emit some lexemes at the end of the input.
  }

-- | A lexer that uses no lexer-modes, and does not emit anything at the
-- end of the file.
simpleLexer :: LexerConfig () t
simpleLexer = LexerConfig
  { lexerInitialState = ()
  , lexerStateMode    = \_ -> 0
  , lexerEOF          = \_ _ -> []
  }


-- | Generate a function to use an Alex lexer.
-- The expression is of type @LexerConfig s t -> Input -> [Lexeme t]@
makeLexer :: ExpQ
makeLexer =
  do let local = do n <- newName "x"
                    return (varP n, varE n)

     ([xP,yP,zP], [xE,yE,zE]) <- unzip <$> replicateM 3 local

     let -- Defined by Alex
         alexEOF        = conP (mkName "AlexEOF")   [ ]
         alexError      = conP (mkName "AlexError") [ wildP ]
         alexSkip       = conP (mkName "AlexSkip")  [ xP, wildP ]
         alexToken      = conP (mkName "AlexToken") [ xP, yP, zP ]
         alexScanUser   = varE (mkName "alexScanUser")

     let p ~> e = match p (normalB e) []
         body go mode inp cfg =
           caseE [| $alexScanUser $mode $inp (lexerStateMode $cfg $mode) |]
             [ alexEOF   ~> [| lexerEOF $cfg $mode (inputPrev $inp) |]
             , alexError ~>
                  [| error "internal error in lexer (AlexToolsBin.hs)" |]
             , alexSkip  ~> [| $go $mode $xE |]
             , alexToken ~> [| case runA $zE $inp $xE $yE $mode of
                                 (mode', ts) -> ts ++ $go mode' $xE |]
             ]

     [e| \cfg -> let go mode inp = $(body [|go|] [|mode|] [|inp|] [|cfg|])
                 in go (lexerInitialState cfg) |]

type AlexInput = Input

{-# INLINE alexInputPrevChar #-}
alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar = toEnum . fromEnum . inputPrevByte

{-# INLINE alexGetByte #-}
alexGetByte :: AlexInput -> Maybe (Word8,AlexInput)
alexGetByte inp =
  do (b,bs) <- BS.uncons (inputBytes inp)
     let inp1 = Input { inputPrev     = inputPos inp
                      , inputPrevByte = b
                      , inputPos      = moveSourcePos (inputPos inp)
                      , inputBytes    = bs
                      }
     inp1 `seq` pure (b,inp1)


