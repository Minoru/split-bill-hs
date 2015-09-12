import Control.Exception
import Control.Monad
import Data.Char
import Data.Decimal
import System.IO

import qualified Data.Map as Map

-- | `AnswersMap` binds textual answers to values of some type so that we can
-- easily pattern-match on them later.
type AnswersMap a = Map.Map Char a


-- ####
-- #### Typical questions with corresponding answers maps
-- ####

data YesNoQuestion = Yes | No

yesNoAnswersMap :: AnswersMap YesNoQuestion
yesNoAnswersMap =
  Map.fromList [ ( 'y', Yes ), ( 'Y', Yes ), ( 'n', No ),  ( 'N', No ) ]


data WhoPaidQuestion = Me | He

whoPaidAnswersMap :: AnswersMap WhoPaidQuestion
whoPaidAnswersMap = Map.fromList [ ('m', Me), ('h', He) ]


data BoughtForWhomQuestion = ForMe | ForHim | ForBoth

boughtForWhomAnswersMap :: AnswersMap BoughtForWhomQuestion
boughtForWhomAnswersMap =
  Map.fromList [ ('m', ForMe), ('h', ForHim), ('b', ForBoth) ]


data CategoryQuestion = Food | Sweets | Misc

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
  , debt   :: Decimal }

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
  category <- ask
    "What category this item belongs to? ([f]ood / [s]weets / [m]isc)"
    categoryAnswersMap

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
  in state


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

-- | Write the transaction into `hledger.journal`
dumpTransaction :: String -> BillProcessingState -> IO ()
dumpTransaction date state = do
  let str = transactionToString
  putStrLn str

  where
    offset = "    "

    transactionToString = unlines $
      [ unwords $ [date, "Сходили с Вадиком в «Сельпо»"]
      , concat $ [ offset, "expenses:food       "
                 , offset, amountToString $ food state ]
      , concat $ [ offset, "expenses:food:sweets"
                 , offset, amountToString $ sweets state ]
      , concat $ [ offset, "expenses:misc       "
                 , offset, amountToString $ misc state ]
      ]
      ++ debit
      ++ credit

    debit =
      if (wallet state < 0)
        then [ concat [ offset, "assets:cash:envelope"
                      , offset, amountToString $ wallet state
                      ]
             ]
        else []

    credit =
      if (debt state /= 0)
        then [ concat [ offset, "assets:loan:vadim   "
                      , offset, amountToString $ debt state
                      ]
             ]
        else []

    amountToString = show

main = whileThereAreBills processBill
