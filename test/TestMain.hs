module Main where

import Test.Tasty
import Test.Tasty.HUnit

import TestHelpers

import SplitBill

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests = testGroup "Unit tests"
  [ iBoughtFoodForMyself
  , iBoughtFoodForHim
  , iBoughtFoodForBoth
  , heBoughtFoodForMe
  , heBoughtFoodForHimself
  , heBoughtFoodForBoth
  ]

iBoughtFoodForMyself =
  let actions = [ (10, ForMe, Food) ]
      expectedState = defaultState {
        food   = 10
      , wallet = -10
      }
  in testCase "I bought food for myself" $
       runActions Me actions @?= expectedState

iBoughtFoodForHim =
      -- we won't be updating any of the "expenses" account, so no need to ask
      -- for category. Thus `undefined`
  let actions = [ (10, ForHim, undefined) ]
      expectedState = defaultState {
        loan   = 10
      , wallet = -10
      }
  in testCase "I bought food for him" $
       runActions Me actions @?= expectedState

iBoughtFoodForBoth =
  let actions = [ (10, ForBoth, Food) ]
      expectedState = defaultState {
        food   = 5
      , loan   = 5
      , wallet = -10
      }
  in testCase "I bought food for both" $
       runActions Me actions @?= expectedState

heBoughtFoodForMe =
  let actions = [ (10, ForMe, Food) ]
      expectedState = defaultState {
        food = 10
      , loan = -10
      }
  in testCase "He bought food for me" $
       runActions He actions @?= expectedState

heBoughtFoodForHimself =
      -- we won't be updating any of the "expenses" account, so no need to ask
      -- for category. Thus `undefined`
  let actions = [ (10, ForHim, undefined) ]
      expectedState = defaultState
  in testCase "He bought food for himself" $
       runActions He actions @?= expectedState

heBoughtFoodForBoth =
  let actions = [ (10, ForBoth, Food) ]
      expectedState = defaultState {
        food   = 5
      , loan   = -5
      }
  in testCase "He bought food for both" $
       runActions He actions @?= expectedState
