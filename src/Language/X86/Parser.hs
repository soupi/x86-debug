
module Language.X86.Parser
  ( parsePrint
  , parseCode
  , parseCodeLine
  , parseErrorPretty
  )
where

import Control.Monad
import Control.Applicative
import Data.Void
import Text.Groom

import qualified Text.Megaparsec as Prs
import qualified Text.Megaparsec.Char as Prs
import qualified Control.Monad.Combinators.Expr as Prs

import Language.X86.Assembly
import Language.X86.Lexer

parseErrorPretty :: Prs.ParseErrorBundle String Void -> String
parseErrorPretty = Prs.errorBundlePretty

parseCode :: String -> String -> Either (Prs.ParseErrorBundle String Void) [Instruction]
parseCode src content =
  Prs.parse (concat <$> many1 (parseInstWithLabel <* lexeme Prs.eol)) src (content ++ "\n")

parseCodeLine :: String -> String -> Either (Prs.ParseErrorBundle String Void) [Instruction]
parseCodeLine src content = Prs.parse parseLine src (content ++ "\n")

parseLine :: Parser [Instruction]
parseLine = parseInstWithLabel <* lexeme Prs.eol

parse :: Parser a -> String -> String -> Either (Prs.ParseErrorBundle String Void) a
parse parser srcName content =
  Prs.parse (parser <* Prs.eof) srcName content

parsePrint :: Show a => Parser a -> String -> IO ()
parsePrint p = putStrLn . either parseErrorPretty groom . parse p "test"

parseLabel :: Parser Instruction
parseLabel =
  fmap Label identifier <* colon

parseArg :: Parser Arg
parseArg =
  (Ref <$> brackets parseArg)
  <|> (AE <$> parseArithExpr parseReg)

parseArithExpr :: Parser var -> Parser (ArithExpr var)
parseArithExpr pvar = expr pvar

parseReg :: Parser Reg
parseReg =
  msum (map (\(s,c) -> symbol s *> pure c) registers)

parseAddress :: Parser Address
parseAddress = parseArithExpr parseAddressVar

parseAddressVar :: Parser AddressVar
parseAddressVar =
  (AR <$> Prs.try parseReg)
  <|> fmap AL identifier

parseBinInstruction :: Parser (Arg -> Arg -> Instruction)
parseBinInstruction =
      (IMov  <$ (rword "mov"  <|> rword "MOV" ))
  <|> (IAdd  <$ (rword "add"  <|> rword "ADD" ))
  <|> (ISub  <$ (rword "sub"  <|> rword "SUB" ))
  <|> (ICmp  <$ (rword "cmp"  <|> rword "CMP" ))
  <|> (IXor  <$ (rword "xor"  <|> rword "XOR" ))
  <|> (IAnd  <$ (rword "and"  <|> rword "AND" ))
  <|> (IOr   <$ (rword "or"   <|> rword "OR"  ))
  <|> (IShl  <$ (rword "shl"  <|> rword "SHL" ))
  <|> (IShr  <$ (rword "shr"  <|> rword "SHR" ))
  <|> (ISar  <$ (rword "sar"  <|> rword "SAR" ))
  <|> (ISal  <$ (rword "sal"  <|> rword "SAL" ))
  <|> (ITest <$ (rword "test" <|> rword "TEST"))

parseJmps :: Parser (Address -> Instruction)
parseJmps =
      (IJmp  <$ (rword "jmp"  <|> rword "JMP"  ))
  <|> (IJe   <$ (rword "je"   <|> rword "JE"   ))
  <|> (IJne  <$ (rword "jne"  <|> rword "JNE"  ))
  <|> (IJnz  <$ (rword "jnz"  <|> rword "JNZ"  ))
  <|> (IJz   <$ (rword "jz"   <|> rword "JZ"   ))
  <|> (IJge  <$ (rword "jge"  <|> rword "JGE"  ))
  <|> (ICall <$ (rword "call" <|> rword "CALL" ))

parseInstruction :: Parser Instruction
parseInstruction =
  (parseBinInstruction <*> (parseArg <* comma) <*> parseArg)
  <|> (parseJmps <*> parseAddress)
  <|> (IMul  <$ (rword "mul"  <|> rword "MUL"  ) <*> parseArg)
  <|> (IPush <$ (rword "push" <|> rword "PUSH" ) <*> parseArg)
  <|> (IPop  <$ (rword "pop"  <|> rword "POP"  ) <*> parseArg)
  <|> (IRet  <$ (rword "ret"  <|> rword "RET"  ))

parseInstWithLabel :: Parser [Instruction]
parseInstWithLabel = do
  Prs.try ((:) <$> parseLabel <*> (parseInstWithLabel <|> fmap pure parseInstruction <|> fmap pure (parseLabel <* lexeme Prs.eol)))
    <|> (fmap pure parseInstruction <|> fmap pure parseLabel)

expr :: Parser var -> Parser (ArithExpr var)
expr pvar = Prs.makeExprParser (term pvar) table

term :: Parser var -> Parser (ArithExpr var)
term pvar =
  parens (expr pvar)
  <|> fmap (Lit . fromIntegral) integer
  <|> fmap Var pvar

table :: [[Prs.Operator Parser (ArithExpr a)]]
table =
  [ [ binary  "*"  Mul ]
  , [ binary  "+" Add
    , binary  "-"  Sub
    ]
  ]

binary :: String
  -> ((ArithExpr a) -> (ArithExpr a) -> (ArithExpr a))
  -> Prs.Operator Parser (ArithExpr a)
binary  name f = Prs.InfixL  (f <$ symbol name)
