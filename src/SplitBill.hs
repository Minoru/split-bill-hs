{-# LANGUAGE ScopedTypeVariables #-}

module SplitBill where

import Control.Exception
import Control.Monad
import Data.Char
import Data.Decimal
import System.Directory
import System.Environment
import System.FilePath
import System.IO

import qualified Data.Map as Map

-- | `AnswersMap` binds textual answers to values of some type so that we can
-- easily pattern-match on them later.
type AnswersMap a = Map.Map Char a


-- ####
-- #### Typical questions with corresponding answers maps
-- ####

data YesNoQuestion = Yes | No
  deriving (Eq, Show)

yesNoAnswersMap :: AnswersMap YesNoQuestion
yesNoAnswersMap =
  Map.fromList [ ( 'y', Yes ), ( 'Y', Yes ), ( 'n', No ),  ( 'N', No ) ]


data WhoPaidQuestion = Me | He
  deriving (Eq, Show)

whoPaidAnswersMap :: AnswersMap WhoPaidQuestion
whoPaidAnswersMap = Map.fromList [ ('m', Me), ('h', He) ]


data BoughtForWhomQuestion = ForMe | ForHim | ForBoth
  deriving (Eq, Show)

boughtForWhomAnswersMap :: AnswersMap BoughtForWhomQuestion
boughtForWhomAnswersMap =
  Map.fromList [ ('m', ForMe), ('h', ForHim), ('b', ForBoth) ]


data CategoryQuestion = Food | Sweets | Misc
  deriving (Eq, Show)

categoryAnswersMap :: AnswersMap CategoryQuestion
categoryAnswersMap =
  Map.fromList [ ('f', Food), ('s', Sweets), ('m', Misc) ]


-- | This functon asks user-specified question as many times as that takes, up
-- until user gives one of the acceptable answers. The value that is paired to
-- the answer in the answers map is returned.
ask :: String -> AnswersMap a -> IO a
ask question answers = withoutBuffering $ do
  putStr question
  unless (last question == ' ') $ putStr " "
  hFlush stdout
  answer <- getChar
  putStrLn ""
  case Map.lookup answer answers of
    Just value -> return value
    Nothing    -> ask question answers

-- ####
-- #### Helper functions that deal with input-output buffering
-- ####

-- | Sets specified buffer mode for the duration of the action, resetting things
-- back to their previous value afterwards
withBufferMode :: BufferMode -> IO a -> IO a
withBufferMode mode action = bracket initialize finalize (const action)
  where
  initialize = do
    bufferMode <- hGetBuffering stdin
    hSetBuffering stdin mode
    return bufferMode

  finalize initialMode = hSetBuffering stdin initialMode

-- | Run action with line I/O buffering
withBuffering :: IO a -> IO a
withBuffering = withBufferMode LineBuffering

-- | Run action without I/O buffering
withoutBuffering :: IO a -> IO a
withoutBuffering = withBufferMode NoBuffering



-- | Run the action, ask if there are more bills to process. If the answer is
-- "yes", repeat, stop otherwise.
whileThereAreBills :: IO () -> IO ()
whileThereAreBills action = do
  action
  answer <- ask "Are there more bills to process? (y/n)" yesNoAnswersMap
  case answer of
    Yes -> do
      putStrLn ""
      whileThereAreBills action
    No  -> return ()

-- | Ask who paid the bill, then process each of the items of the bill and dump
-- the resulting transaction into `hledger.journal`.
processBill :: IO ()
processBill = do
  date  <- askForBillDate
  payee <- ask "Who paid for this bill? ([m]e/[h]e)" whoPaidAnswersMap
  state <- whileThereAreItems payee processItem
  dumpTransaction date state

-- | A piece of state that we'll be carrying while processing items
data BillProcessingState = BillProcessingState {
    -- | How much went into expenses:food
    food   :: Decimal
    -- | How much went into expenses:food:sweets
  , sweets :: Decimal
    -- | How much went into expenses:misc
  , misc   :: Decimal
    -- | How much I withdrew from assets:cash:envelope
  , wallet :: Decimal
    -- | How much I lent to or borrowed from assets:loans:vadim
  , loan   :: Decimal }
  deriving (Eq, Show) -- need those for tests

whileThereAreItems :: WhoPaidQuestion
                   -> (WhoPaidQuestion -> BillProcessingState
                       -> IO BillProcessingState)
                   -> IO BillProcessingState
whileThereAreItems payee action = helper (BillProcessingState 0 0 0 0 0)
  where
  helper state = do
    state' <- action payee state
    answer <- ask "Is that all for this bill? (y/n)" yesNoAnswersMap
    case answer of
      Yes -> return state'
      No  -> helper state'

-- | Update processing state accordingly to how much the item cost, who will use
-- it and what category it belonds to
processItem :: WhoPaidQuestion -> BillProcessingState
            -> IO BillProcessingState
processItem payee state = do
  cost <- askForCost
  boughtFor <- ask "Bought for whom? ([m]e / [h]im / [b]oth)"
                   boughtForWhomAnswersMap
  category <-
    if (boughtFor /= ForHim)
      then
        ask
          "What category this item belongs to? ([f]ood / [s]weets / [m]isc)"
          categoryAnswersMap
      else return undefined

  return $ processItem' state payee cost boughtFor category

processItem' :: BillProcessingState
             -> WhoPaidQuestion
             -> Decimal
             -> BoughtForWhomQuestion
             -> CategoryQuestion
             -> BillProcessingState
processItem' state payee cost boughtFor category =
  let cost' = case boughtFor of
                ForMe   -> cost
                ForHim  -> 0
                ForBoth -> cost / 2

      loan' = case payee of
                Me -> case boughtFor of
                        ForMe   -> 0
                        ForHim  -> cost
                        ForBoth -> cost'
                He -> case boughtFor of
                        ForMe   -> negate cost
                        ForHim  -> 0
                        ForBoth -> negate cost'


      state' = if (boughtFor /= ForHim)
                 then incrementCategory category cost' state
                 else state
  in state' {
       wallet = wallet state' - if (payee == Me) then cost else 0
     , loan   = loan   state' + loan'
     }

  where
    incrementCategory Food   cost state = state { food   = food   state + cost }
    incrementCategory Sweets cost state = state { sweets = sweets state + cost }
    incrementCategory Misc   cost state = state { misc   = misc   state + cost }


-- | Trim whitespace (' ', \t, \n, \r, \f, \v) from both ends of the line
--
-- Highly inefficient, don't use on huge strings!
trim :: String -> String
trim str =
  let dropSpaces = dropWhile isSpace
  in reverse $ dropSpaces $ reverse $ dropSpaces str

-- | Ask for the bill's date in YYYY/MM/DD format
askForBillDate :: IO String
askForBillDate = withBuffering $ do
  putStr "Date (YYYY/MM/DD): "
  hFlush stdout
  date <- liftM trim getLine
  if (isMalformed date)
    then askForBillDate
    else return date

  where
    -- | The date should be in YYYY/MM/DD format
    isMalformed :: String -> Bool
    isMalformed s =
      not $
         (length s == 10)
      && (all isDigit $ take 4 s)
      && (s !! 4 == '/')
      && (all isDigit $ take 2 $ drop 5 s)
      && (s !! 7 == '/')
      && (all isDigit $ take 2 $ drop 8 s)

-- | Ask user how much an item cost
askForCost :: IO Decimal
askForCost = withBuffering $ do
  putStr "Cost: "
  hFlush stdout
  cost <- liftM trim getLine
  if (isMalformed cost)
    then askForCost
    else return $ read cost

  where
    -- | The cost is valid if it's a series of digits followed by a dot and
    -- another series of digits. The latter shouldn't be longer than two chars
    isMalformed :: String -> Bool
    isMalformed str =
      let str' = dropWhile isDigit str
      in not $
           (null str')
        || (  (head str' == '.')
           && (all isDigit $ tail str')
           && (length (tail str') <= 2)
           )

-- | Get the default journal file path specified by the environment.
-- Like ledger, we look first for the LEDGER_FILE environment
-- variable, and if that does not exist, for the legacy LEDGER
-- environment variable. If neither is set, or the value is blank,
-- return the hard-coded default, which is @.hledger.journal@ in the
-- users's home directory (or in the current directory, if we cannot
-- determine a home directory).
--
-- Shamelessly stolen from hledger's code.
defaultJournalPath :: IO String
defaultJournalPath = do
  s <- envJournalPath
  if null s then defaultJournalPath else return s
    where
      envJournalPath =
        getEnv "LEDGER_FILE"
         `catch` (\(_::IOException) -> getEnv "LEDGER"
                                            `catch` (\(_::IOException) -> return ""))
      defaultJournalPath = do
                  home <- getHomeDirectory `catch` (\(_::IOException) -> return "")
                  return $ home </> ".hledger.journal"

-- | Write the transaction into `hledger.journal`
dumpTransaction :: String -> BillProcessingState -> IO ()
dumpTransaction date state = do
  let str = transactionToString

  putStrLn ""
  putStrLn str

  good <- withoutBuffering $
    ask
      "Write this transaction to the journal? (y/n) "
      yesNoAnswersMap

  case good of
    Yes -> do
      path <- defaultJournalPath
      putStrLn $ "Writing to " ++ path
      appendFile path ("\n" ++ str)
    No  -> return ()

  where
    offset = "    "

    transactionToString = unlines $
      [ unwords $ [date, "Сходили с Вадиком в «Сельпо»"] ]
      ++ food'
      ++ sweets'
      ++ misc'
      ++ debit
      ++ credit

    food' =
      if (food state /= 0)
        then [ concat $ [ offset, "expenses:food       "
                        , offset, amountToString $ food state ] ]
        else []

    sweets' =
      if (sweets state /= 0)
        then [ concat $ [ offset, "expenses:food:sweets"
                        , offset, amountToString $ sweets state ] ]
        else []

    misc' =
      if (misc state /= 0)
        then [ concat $ [ offset, "expenses:misc       "
                        , offset, amountToString $ misc state ] ]
        else []

    debit =
      if (wallet state < 0)
        then [ concat [ offset, "assets:cash:envelope"
                      , offset, amountToString $ wallet state
                      ]
             ]
        else []

    credit =
      if (loan state /= 0)
        then [ concat [ offset, "assets:loan:vadim   "
                      , offset, amountToString $ loan state
                      ]
             ]
        else []

    amountToString = show
