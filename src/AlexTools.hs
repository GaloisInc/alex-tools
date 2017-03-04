{-# language TemplateHaskell, CPP, RecordWildCards #-}
module AlexTools
  ( -- * Lexer Basics
    initialInput, Input(..)
  , Lexeme(..)
  , SourcePos(..), startPos, beforeStartPos
  , SourceRange(..)
  , prettySourcePos, prettySourceRange
  , HasRange(..)
  , (<->)
  , moveSourcePos

    -- * Writing Lexer Actions
  , Action

    -- ** Lexemes
  , lexeme
  , matchLength
  , matchRange
  , matchText

    -- ** Manipulating the lexer's state
  , getLexerState
  , setLexerState

    -- ** Access to the lexer's input
  , startInput
  , endInput

    -- * Interface with Alex
  , AlexInput
  , alexInputPrevChar
  , makeAlexGetByte
  , makeLexer
  , LexerConfig(..)
  , simpleLexer
  , Word8

    -- * Helpers for writing Layout
  , Layout(..)
  , layout

  ) where

import           Control.DeepSeq
import           Data.Word(Word8)
import           Data.Text(Text)
import qualified Data.Text as Text
import           Control.Monad(liftM,ap,replicateM)
import           Language.Haskell.TH
#if !MIN_VERSION_base(4,8,0)
import           Control.Applicative
#endif

data Lexeme t = Lexeme
  { lexemeText  :: !Text
  , lexemeToken :: !t
  , lexemeRange :: !SourceRange
  } deriving (Show, Eq)

instance NFData t => NFData (Lexeme t) where
  rnf (Lexeme x y z) = rnf (x,y,z)

data SourcePos = SourcePos
  { sourceIndex   :: !Int
  , sourceLine    :: !Int
  , sourceColumn  :: !Int
  } deriving (Show, Eq)

prettySourcePos :: SourcePos -> String
prettySourcePos x = show (sourceLine x) ++ ":" ++ show (sourceColumn x)


instance NFData SourcePos where
  rnf (SourcePos x y z) = rnf (x,y,z)

-- | Update a 'SourcePos' for a particular matched character
moveSourcePos :: Char -> SourcePos -> SourcePos
moveSourcePos c p = SourcePos { sourceIndex  = sourceIndex p + 1
                              , sourceLine   = newLine
                              , sourceColumn = newColumn
                              }
  where
  line   = sourceLine p
  column = sourceColumn p

  (newLine,newColumn) = case c of
                          '\t' -> (line, ((column + 7) `div` 8) * 8 + 1)
                          '\n' -> (line + 1, 1)
                          _    -> (line, column + 1)


-- | A range in the source code.
data SourceRange = SourceRange
  { sourceFrom :: !SourcePos
  , sourceTo   :: !SourcePos
  } deriving (Show, Eq)

prettySourceRange :: SourceRange -> String
prettySourceRange x = prettySourcePos (sourceFrom x) ++ "--" ++
                      prettySourcePos (sourceTo x)

instance NFData SourceRange where
  rnf (SourceRange x y) = rnf (x,y)

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
matchText :: Action s Text
matchText =
  do i1 <- startInput
     n  <- matchLength
     return (Text.take n (inputText i1))

-- | Use the token and the current match to construct a lexeme.
lexeme :: t -> Action s [Lexeme t]
lexeme tok =
  do r   <- matchRange
     txt <- matchText
     return [ Lexeme { lexemeRange = r
                     , lexemeToken = tok
                     , lexemeText  = txt
                     } ]

-- | Information about the lexer's input.
data Input = Input
  { inputPos      :: {-# UNPACK #-} !SourcePos
    -- ^ Current input position.

  , inputText     :: {-# UNPACK #-} !Text
    -- ^ The text that needs to be lexed.

  , inputPrev     :: {-# UNPACK #-} !SourcePos
    -- ^ Location of the last consumed character.

  , inputPrevChar :: {-# UNPACK #-} !Char
    -- ^ The last consumed character.
  }

-- | Prepare the text for lexing.
initialInput :: Text -> Input
initialInput str = Input
  { inputPos      = startPos
  , inputPrev     = beforeStartPos
  , inputPrevChar = '\n'    -- end of the virtual previous line
  , inputText     = str
  }

startPos :: SourcePos
startPos = SourcePos { sourceIndex   = 0
                     , sourceLine    = 1
                     , sourceColumn  = 1
                     }

beforeStartPos :: SourcePos
beforeStartPos = SourcePos { sourceIndex   = -1
                           , sourceLine    = 0
                           , sourceColumn  = 0
                           }


--------------------------------------------------------------------------------
-- | Lexer configuration.
data LexerConfig s t = LexerConfig
  { lexerInitialState :: s
    -- ^ State that the lexer starts in

  , lexerStateMode :: s -> Int
    -- ^ Determine the current lexer mode from the lexer's state.

  , lexerEOF       :: s -> [Lexeme t]
    -- ^ Emit some lexemes at the end of the input.
  }

-- | A lexer that uses no lexer-modes, and does not emit anything at the
-- end of the file.
simpleLexer :: LexerConfig () t
simpleLexer = LexerConfig
  { lexerInitialState = ()
  , lexerStateMode = \_ -> 0
  , lexerEOF       = \_ -> []
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
             [ alexEOF   ~> [| lexerEOF $cfg $mode |]
             , alexError ~> [| error "internal error in lexer (AlexTools.hs)" |]
             , alexSkip  ~> [| $go $mode $xE |]
             , alexToken ~> [| case runA $zE $inp $xE $yE $mode of
                                 (mode', ts) -> ts ++ $go mode' $xE |]
             ]

     [e| \cfg -> let go mode inp = $(body [|go|] [|mode|] [|inp|] [|cfg|])
                 in go (lexerInitialState cfg) |]

type AlexInput = Input

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar = inputPrevChar

{-# INLINE makeAlexGetByte #-}
makeAlexGetByte :: (Char -> Word8) -> AlexInput -> Maybe (Word8,AlexInput)
makeAlexGetByte charToByte Input { inputPos = p, inputText = text } =
  do (c,text') <- Text.uncons text
     let p'  = moveSourcePos c p
         x   = charToByte c
         inp = Input { inputPrev     = p
                     , inputPrevChar = c
                     , inputPos      = p'
                     , inputText     = text'
                     }
     x `seq` inp `seq` return (x, inp)

-- | The 'Layout' type describes how how to run the layout rule for
--   a given sequence of tokens.
data Layout tok = Layout
  { beginsLayout :: tok -> Bool
    -- ^ True when this token begins layout
  , endsLayout :: tok -> Bool
    -- ^ True when this token explicitly ends layout
  , sepToken :: SourceRange -> Lexeme tok
    -- ^ The separator token
  , startToken :: SourceRange -> Lexeme tok
    -- ^ Layout block starting token
  , endToken :: SourceRange -> Lexeme tok
    -- ^ Layout block ending token
  }

-- | Perform the layout algorithm for indentation-sensitive parsing.
-- This uses a simple algorithm analogous to Haskell's algorithm described
-- <https://www.haskell.org/onlinereport/lexemes.html#sect2.7 in the Haskell report>,
-- which can insert synthetic delimiter and sepearator tokens based on
-- the relative indentations of the tokens in a token stream.
--
-- The 'Layout' type lets you specify which tokens trigger insertion of
-- a synthetic open bracket, which trigger insertion of a synthetic close
-- bracket, and which tokens to use for the synthetic open brackets, close
-- backets, and semicolon. The resulting token stream is unmodified except
-- for the insertion of the layout-related tokens.
layout :: Layout t -> [Lexeme t] -> [Lexeme t]
layout Layout { .. } = go Nothing []
  where
  startCol SourceRange { sourceFrom = SourcePos { .. } } = sourceColumn

  currentLevel (loc : _) = startCol loc
  currentLevel []        = 0

  getRange Lexeme { lexemeRange = l } = l

  -- a new layout level has been started, emit a starting token, and push the
  -- current level on the stack.
  go Just{} stack (tok : toks) =
    let loc = getRange tok
    in startToken loc : tok : go Nothing (loc:stack) toks

  go (Just loc) stack [] =
    startToken loc : go Nothing (loc : stack) []

  go Nothing stack ts@(tok : toks)

    -- when the next token would close the current level
    | startCol loc < currentLevel stack =
      endToken loc : go Nothing (tail stack) ts

    | beginsLayout val =
      let sepToks | startCol loc == currentLevel stack = [sepToken loc]
                  | otherwise                          = []
       in sepToks ++ tok : go (Just loc) stack toks

    | endsLayout val =
      endToken loc : tok : go Nothing (tail stack) toks

    | startCol loc == currentLevel stack =
      sepToken loc : tok : go Nothing stack toks

    | otherwise =
      tok : go Nothing stack toks
    where Lexeme { lexemeRange = loc, lexemeToken = val } = tok

  go _ stack [] =
    [ endToken loc | loc <- stack ]
