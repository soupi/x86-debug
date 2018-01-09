{-# LANGUAGE NamedFieldPuns #-}

module Language.X86.Run where

import Language.X86

import Data.Char
import Data.Foldable
import Data.Bifunctor
import System.Exit
import System.IO

run :: IO ()
run = do
  putStrLn hello
  hSetBuffering stdout NoBuffering
  repl NoState
  putStrLn "Bye!"


hello :: String
hello = unlines $
  [ "x86-debug - a (subset of) x86 debugger"
  , "Type ? for help"
  ]

help :: String
help = unlines $
  "Available commands are:\n"
  : map (("* " ++) . fst) commands'

data ReplState
  = NoState
  | ReplMachineState State

repl :: ReplState -> IO ()
repl s = do
  putStr "> "
  words <$> getLine >>= \case
    [] -> repl s
    cmd:args  -> case lookup cmd commands of
      Nothing -> do
        hPutStrLn stderr $ "Command not understood: '" ++ cmd ++ "'."
        repl s
      Just command ->
        command args s

commands :: [(String, [String] -> ReplState -> IO ())]
commands =
  commands' ++ map (first (':':)) commands'

commands' :: [(String, [String] -> ReplState -> IO ())]
commands' =
  [ ( "quit"
    , const $ const $ pure ()
    )

  , ( "?"
    , \_ s -> do
        putStrLn help
        repl s
    )
  , ( "init"
    , const $ initReplMachineState
    )
  , ( "code"
    , const $ ppCode
    )

  , ( "regs"
    , readReg
    )

  , ( "reg"
    , readReg
    )

  , ( "next"
    , const $ runNext
    )
  ]

initReplMachineState :: ReplState -> IO ()
initReplMachineState s = do
  putStrLn "Please enter code. To mark you are done write 'done'. "
  maybeCode <- readCode
  case maybeCode of
    Nothing -> do
      hPutStrLn stderr "Failed to parse code. Reverting to last state."
      repl s
    Just code -> do
      putStrLn "Code parsed successfully. To view it type 'code'."
      repl $ ReplMachineState [initMachine code]

readCode :: IO (Maybe Code)
readCode = do
  go [] >>= \case
    [] -> do
      putStrLn "Doing nothing."
      pure Nothing
    code ->
      pure $ Just $ toCode [] code

  where
    go code = do
      putStr "...> "
      getLine >>= pure . trim >>= \case
        "quit" -> do
          putStrLn "Bye!"
          exitSuccess
        "discard" ->
          pure []
        "done" ->
          pure $ reverse code
        line -> do
          case readMaybe line of
            Just c -> go (c : code)
            _ -> do
              hPutStrLn stderr "Failed to read line."
              go code


ppCode :: ReplState -> IO ()
ppCode s = do
  case s of
    NoState ->
      hPutStrLn stderr "No code to show yet. Use 'init' to insert code."
    ReplMachineState state -> case state of
      [] ->
        hPutStrLn stderr "No state available. Use 'init' to insert code."
      Machine{mCode} : _ ->
        putStrLn
          . ppAsm
          . toList
          . fmap lineInst
          . cCode
          $ mCode

  repl s

readReg :: [String] -> ReplState -> IO ()
readReg rs s = do
  case (map (map toUpper) rs, s) of
    (_, NoState) ->
      hPutStrLn stderr "You need to init the machine first."
    (_, ReplMachineState []) ->
      hPutStrLn stderr "Invalid state."
    ([], _) ->
      hPutStrLn stderr "No registers requested."
    (regs, ReplMachineState (machine:_)) -> do
      let results = map (\reg -> (reg,) $ (`getReg` machine) <$> readMaybe reg) regs
      forM_ results $ \(reg, result) ->
        putStrLn
          $ reg ++ " = " ++
            case result of
              Nothing ->
                "Unknown register"
              Just v ->
                show v
  repl s


readMaybe :: Read a => String -> Maybe a
readMaybe s = case reads s of
  [(a, "")] -> Just a
  _ -> Nothing

trim :: String -> String
trim = unwords . words

runNext :: ReplState -> IO ()
runNext = \case
  NoState -> do
    hPutStrLn stderr "You need to init a machine first."
    repl NoState
  ReplMachineState [] -> do
    hPutStrLn stderr "Invalid state. Setting to no state."
    repl NoState
  ReplMachineState machines@(machine:_) -> do
    case (isHalt machine, stepForward machine) of
      (Right True, _) -> do
        putStrLn "Halted."
        repl $ ReplMachineState machines
      (_, Right m') ->
        repl $ ReplMachineState (m' : machines)
      (_, Left (Error _ str _ errtype)) ->
        hPutStrLn stderr $ unwords
          [ "*** Error in "
          , show str
          , show errtype
          ]
