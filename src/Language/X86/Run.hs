{-# LANGUAGE NamedFieldPuns #-}

module Language.X86.Run where

import Language.X86

import Prelude hiding (break)
import Data.Int (Int32)
import Data.Char
import Data.Maybe
import Data.Functor
import Data.Foldable
import Data.Bifunctor
import Data.Traversable
import Control.Applicative
import System.Exit
import System.IO
import Text.Groom

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
  | Done
  deriving Show

repl :: ReplState -> IO ()
repl s = do
  putStr "> "
  words <$> getLine >>= \case
    [] -> repl s
    cmd:args  -> case lookup cmd commands of
      Nothing -> do
        hPutStrLn stderr $ "Command not understood: '" ++ cmd ++ "'."
        repl s
      Just command -> do
        rs <- command args s
        case rs of
          Done -> pure ()
          _ -> repl rs

commands :: [(String, [String] -> ReplState -> IO ReplState)]
commands =
  commands'
    ++ map (first (':':)) commands'
    ++ map (first $ (:[]) . head) commands'
    ++ map (first $ (\x -> [':',x]) . head) commands'

commands' :: [(String, [String] -> ReplState -> IO ReplState)]
commands' =
  [ ( "quit"
    , const $ const $ pure Done
    )

  , ( "help"
    , \_ s -> do
        putStrLn help
        pure s
    )

  , ( "?"
    , \_ s -> do
        putStrLn help
        pure s
    )

  , ( "init"
    , const $ initReplMachineState
    )

  , ( "code"
    , \_ s -> s <$ printCode s
    )

  , ( "line"
    , \_ s -> s <$ printLine s
    )

  , ( "regs"
    , \args s -> s <$ readReg args s
    )

  , ( "reg"
    , \args s -> s <$ readReg args s
    )

  , ( "next"
    , const $ runNext interpretStep
    )

  , ( "step"
    , const $ runNext interpretBreak
    )

  , ( "run"
    , const $ runNext interpret
    )

  , ( "prev"
    , const $ runPrev
    )

  , ( "previous"
    , const $ runPrev
    )

  , ( "break"
    , handleBreak addBreakpointLine "added" "adding"
    )

  , ( "unbreak"
    , handleBreak removeBreakpointLine "delete" "deleting"
    )


  , ( "start"
    , \_ s -> case s of
        ReplMachineState ms@(_:_) ->
          pure $ ReplMachineState [last ms]
        _ -> do
          hPutStrLn stderr "You need to init the machine first."
          pure s
    )

  , ( "machine"
    , \_ s -> do
        putStrLn $ groom s
        pure s
    )

  ]

initReplMachineState :: ReplState -> IO ReplState
initReplMachineState s = do
  putStrLn "Please enter code. To mark you are done write 'done'. "
  maybeCode <- readCode
  case maybeCode of
    Nothing -> do
      hPutStrLn stderr "Failed to parse code. Reverting to last state."
      pure s
    Just code -> do
      putStrLn "Code parsed successfully. To view it type 'code'."
      pure $ ReplMachineState [initMachine code]

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


printCode :: ReplState -> IO ReplState
printCode s = do
  case s of
    Done -> pure ()
    NoState ->
      hPutStrLn stderr "No code to show yet. Use 'init' to insert code."
    ReplMachineState state -> case state of
      [] ->
        hPutStrLn stderr "No state available. Use 'init' to insert code."
      Machine{mCode} : _ ->
        putStrLn
          . ppCode
          $ mCode

  pure s

printLine :: ReplState -> IO ReplState
printLine s = do
  case s of
    Done -> pure ()
    NoState ->
      hPutStrLn stderr "No code to show yet. Use 'init' to insert code."
    ReplMachineState state -> case state of
      [] ->
        hPutStrLn stderr "No state available. Use 'init' to insert code."
      m : _ -> do
        case getInstLine m of
          Left (Error _ str _ errtype) -> do
            hPutStrLn stderr $ unwords
              [ "*** Error in "
              , show str
              , show errtype
              ]
          Right line ->
            putStrLn $ concat
              [ show $ lineAnn line
              , ":"
              , ppInstruction $ lineInst line
              ]

  pure s

readReg :: [String] -> ReplState -> IO ReplState
readReg rs s = do
  case (map (map toUpper) rs, s) of
    (_, Done) -> pure ()
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
  pure s


readMaybe :: Read a => String -> Maybe a
readMaybe s = case reads s of
  [(a, "")] -> Just a
  _ -> Nothing

trim :: String -> String
trim = unwords . words

runNext :: (State -> Either Error State) -> ReplState -> IO ReplState
runNext runner = \case
  NoState -> do
    hPutStrLn stderr "You need to init a machine first."
    pure NoState

  ReplMachineState [] -> do
    hPutStrLn stderr "Invalid state. Setting to no state."
    pure NoState

  ReplMachineState machines@(machine:_) -> do
    case (isHalt machine, runner machines) of
      (Right True, _) -> do
        putStrLn "Already halted."
        pure $ ReplMachineState machines

      (_, Right m') -> do
        case (,) <$> isHalt (head m') <*> isBreakpoint (head m') of
          Right (True, _) ->
            putStrLn "Halted."
          Right (False, True) -> do
            putStrLn $ "Breakpoint reached."
            printLine (ReplMachineState m') $> ()
          _ ->
            printLine (ReplMachineState m') $> ()
        pure $ ReplMachineState m'

      (_, Left (Error _ str _ errtype)) -> do
        hPutStrLn stderr $ unwords
          [ "*** Error in "
          , show str
          , show errtype
          ]
        pure $ ReplMachineState machines

  s -> pure s

runPrev :: ReplState -> IO ReplState
runPrev = \case
  NoState -> do
    hPutStrLn stderr "You need to init a machine first."
    pure NoState
  ReplMachineState [] -> do
    hPutStrLn stderr "Invalid state. Setting to no state."
    pure NoState
  ReplMachineState machines@(_:[]) -> do
    putStrLn "Already at the beginning."
    _ <- printLine (ReplMachineState machines)
    pure $ ReplMachineState machines
  ReplMachineState (_:rest) -> do
    _ <- printLine (ReplMachineState rest)
    pure $ ReplMachineState rest
  s -> pure s


handleBreak :: (Int32 -> Machine -> Either [Char] Machine)
  -> String -> String -> [String] -> ReplState -> IO ReplState
handleBreak handle addrmed addrming bps s =
  case (bps, s) of
    (_, Done) -> pure s
    (_, NoState) -> do
      hPutStrLn stderr "You need to init the machine first."
      pure s
    (_, ReplMachineState []) -> do
      hPutStrLn stderr "Invalid state."
      pure s
    ([], _) -> do
      hPutStrLn stderr "No breakpoints requested."
      pure s
    (_, ReplMachineState machines@(machine:_)) -> do
      let breaks = map (\b -> (b, readBreakpoint machine b)) bps
      forM_ breaks $ \(break, line) -> do
        case line of
          Just _ -> pure ()
          Nothing ->
            hPutStrLn stderr $ "Unknown breakpoint: " ++ break
      let possiblebreaks = filter (isJust . snd) breaks
      (catMaybes -> truebreaks) <-
        forM possiblebreaks $ \(break, Just line) ->
          case handle line machine of
            Right _ -> do
              putStrLn $ "Breakpoint " ++ addrmed ++ " at line: " ++ show line ++ "."
              pure $ Just line
            Left err -> do
              hPutStrLn stderr $ "Error " ++ addrming ++ " breakpoint " ++ break ++ ": " ++ err
              pure $ Nothing

      if null truebreaks
        then
          pure $ ReplMachineState machines
        else
          pure $ ReplMachineState
            [ either (const m) id $ handle l m
            | m <- machines
            , l <- truebreaks
            ]

readBreakpoint :: Machine -> String -> Maybe Int32
readBreakpoint machine b =
  (readMaybe (map toUpper b) >>= pure . flip getReg machine)
  <|> readMaybe b
  <|> getLabelLineNum b machine
