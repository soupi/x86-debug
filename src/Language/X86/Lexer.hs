{-# LANGUAGE TypeApplications #-}

module Language.X86.Lexer where

import Data.Void
import Control.Arrow ((&&&), first)
import Data.Data (showConstr, toConstr, dataTypeConstrs, dataTypeOf)
import Data.Char (toUpper, toLower)
import Data.Functor
import Control.Applicative ((<|>))

import Language.X86.Assembly

import qualified Text.Megaparsec as Prs
import qualified Text.Megaparsec.Char.Lexer as Lex
import qualified Text.Megaparsec.Char as Prs

type Parser = Prs.Parsec Void String

-- | Defining what is considered a space to consume
spaceConsumer :: Parser ()
spaceConsumer = Lex.space (Prs.skipSome (void Prs.tab <|> void (Prs.char ' '))) lineCmnt blockCmnt
  where
    lineCmnt  = Lex.skipLineComment ";"
    blockCmnt = Lex.skipBlockCommentNested "/*" "*/"

lexeme :: Parser a -> Parser a
lexeme = Lex.lexeme spaceConsumer

many1 :: Parser a -> Parser [a]
many1 p = (:) <$> p <*> Prs.many p

symbol :: String -> Parser String
symbol = lexeme . Lex.symbol spaceConsumer

-- | 'integer' parses an integer
integer :: Parser Integer
integer = lexeme $
  (((Prs.char '-') $> negate) <|> pure id)
  <*> ((Prs.try (Prs.char '0' *> Prs.char 'x') *> Lex.hexadecimal)
       <|> Lex.decimal
      )

-- | strings
string :: Parser String
string = lexeme $ Prs.char '"' >> Prs.manyTill Lex.charLiteral (Prs.char '"')

-- | char
char :: Parser Char
char = lexeme $ Prs.char '\'' *> Lex.charLiteral <* Prs.char '\''

rword :: String -> Parser ()
rword w = Prs.string w *> Prs.notFollowedBy Prs.alphaNumChar *> spaceConsumer

-- | list of reserved words
reservedWords :: [String]
reservedWords =
  map (map toLower) instructions
  ++ map (map toUpper) instructions

instructions :: [String]
instructions =
  map tail
  . filter (\(c:_) -> c == 'I')
  . map showConstr
  . dataTypeConstrs
  . dataTypeOf @Instruction
  $ undefined

registers :: [(String, Reg)]
registers =
  regs ++ map (first (map toLower)) regs
    where
      regs =
        map
          (showConstr . toConstr &&& id @Reg)
          [minBound..maxBound]

-- | identifiers
identifier :: Parser String
identifier = lexeme (many1 chara >>= check)
  where
    check x = if x `elem` reservedWords
                then fail $ "instruction " ++ show x ++ " cannot be an identifier"
                else pure x
    chara  = Prs.alphaNumChar <|> Prs.oneOf ("/_-" :: String)



-- | 'parens' parses something between parenthesis
parens :: Parser a -> Parser a
parens = Prs.between (symbol "(") (symbol ")")

braces, angles, brackets :: Parser a -> Parser a
braces    = Prs.between (symbol "{") (symbol "}")
angles    = Prs.between (symbol "<") (symbol ">")
brackets  = Prs.between (symbol "[") (symbol "]")

semicolon, comma, colon, dot, equals, arrow, lambda, tilda :: Parser String
semicolon = symbol ";"
comma     = symbol ","
colon     = symbol ":"
dot       = symbol "."
equals    = symbol "="
arrow     = symbol "->"
lambda    = symbol "\\"
tilda     = symbol "~"
