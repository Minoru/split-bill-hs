import Control.Exception
import Control.Monad
import Data.Decimal
import System.IO

import Data.Map as Map

-- | `AnswersMap` binds textual answers to values of some type so that we can
-- easily pattern-match on them later.
type AnswersMap a = Map Char a


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
    Yes -> whileThereAreBills action
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
  cost' <- case boughtFor of
    ForMe   -> return   cost
    ForHim  -> return   0
    ForBoth -> return $ cost / 2
  return state

askForBillDate = undefined

dumpTransaction = undefined

askForCost = undefined

main = whileThereAreBills processBill
