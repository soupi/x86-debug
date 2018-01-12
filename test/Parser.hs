module Parser where

import Language.X86

import Testing
import Simple


tests :: TestTree
tests =
  testGroup "Parser" $
    mconcat
      [ zipWith (\n t -> testCase ("Simple " ++ show n) t) [1..] $ map ppParsePPparse simple
      , zipWith (\n t -> testCase ("Jumps  " ++ show n) t) [1..] $ map ppParsePPparse jumps
      , zipWith (\n t -> testCase ("Stack  " ++ show n) t) [1..] $ map ppParsePPparse stack
      , zipWith (\n t -> testCase ("Calls  " ++ show n) t) [1..] $ map ppParsePPparse calls
      ]

ppParsePPparse (snd -> insts) =
    case ppppp of
      Right _ -> pure ()
      Left er ->
        errorWithoutStackTrace $
          "Test failed to parse: " ++ parseErrorPretty er
  where
    ppppp = do
      i' <- parseCode "test1" (ppAsm insts)
      parseCode "test2" (ppAsm i')
      pure ()


