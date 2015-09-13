module TestHelpers where

import Data.Decimal
import Data.List

import SplitBill

defaultState :: BillProcessingState
defaultState = BillProcessingState 0 0 0 0 0

runActions :: WhoPaidQuestion
           -> [ (Decimal, BoughtForWhomQuestion, CategoryQuestion) ]
           -> BillProcessingState
runActions payee actions =
  let fn = \state (x, y, z) -> processItem' state payee x y z
  in foldl' fn defaultState actions
