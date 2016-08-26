{-# LANGUAGE TemplateHaskell #-}
module AlexTools
  ( -- * Lexer Basics
    initialInput, Input(..)
  , Lexeme(..)
  , SourcePos(..)
  , SourceRange(..)
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

  ) where

import           Data.Word(Word8)
import           Data.Text(Text)
import qualified Data.Text as Text
import           Control.Monad(liftM,ap,replicateM)
import           Language.Haskell.TH

data Lexeme t = Lexeme
  { lexemeText  :: !Text
  , lexemeToken :: !t
  , lexemeRange :: !SourceRange
  } deriving Show

data SourcePos = SourcePos
  { sourceIndex   :: !Int
  , sourceLine    :: !Int
  , sourceColumn  :: !Int
  } deriving Show

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
  } deriving Show

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
  { inputPos      = start
  , inputPrev     = preStart
  , inputPrevChar = '\n'    -- end of the virtual previous line
  , inputText     = str
  }
  where
  start = SourcePos { sourceIndex   = 0
                    , sourceLine    = 1
                    , sourceColumn  = 1
                    }

  preStart = SourcePos { sourceIndex   = -1
                       , sourceLine    = 0
                       , sourceColumn  = 0
                       }



--------------------------------------------------------------------------------
-- | Lexer configuration.
data LexerConfig s t = LexerConfig
  { lexerStateMode :: s -> Int
    -- ^ Determine the current lexer mode from the lexer's state.

  , lexerEOF       :: s -> [Lexeme t]
    -- ^ Emit some lexemes at the end of the input.
  }

-- | A lexer that uses no lexer-modes, and does not emit anything at the
-- end of the file.
simpleLexer :: LexerConfig s t
simpleLexer = LexerConfig
  { lexerStateMode = \_ -> 0
  , lexerEOF       = \_ -> []
  }


-- | Generate a function to use an Alex lexer.
-- The expression is of type @LexerConfig s t -> Input -> s -> [Lexeme t]@
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


     [e| \cfg ->

         let go inp mode =
               case $alexScanUser mode inp (lexerStateMode cfg mode) of
                 $alexEOF   -> lexerEOF cfg mode
                 $alexError -> error "language-lua lexer internal error"
                 $alexSkip  -> go $xE mode
                 $alexToken -> case runA $zE inp $xE $yE mode of
                                 (mode', ts) -> ts ++ go $xE mode'
         in go

      |]

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



