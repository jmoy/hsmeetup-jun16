module Main where

import Data.HashMap.Strict 
        ((!),HashMap,fromList,toList)
import Control.Monad.ST
import Data.STRef

-- Customer name
type Name = String
-- Who owns how much
type Ledger = [(Name,Double)]
-- A transfer from one customer to another
data Transaction = Transaction 
                      Name --From
                      Name --To
                      Double --Amount

-- Given a starting Ledger and a list of 
-- transactions, we want to compute the final
-- state of the Ledger.
start::Ledger
start = [("A",0),("B",0),("C",0)]

transacts::[Transaction]
transacts = [Transaction "A" "B" 10
            ,Transaction "B" "C" 10
            ]

main::IO ()
main = print $ 
        simulateBank start transacts

simulateBank::Ledger->[Transaction]->Ledger
simulateBank l0 ts = runST $ do
  b <- mkBank l0
  mapM_ (transact b) ts
  readLedger b

type Bank s = HashMap Name (STRef s Double)

mkBank::[(Name,Double)]->ST s (Bank s)
mkBank custs = do
  let (n,as) = unzip custs
  vs <- mapM newSTRef as
  return $ fromList $ zip n vs

transact::Bank s->Transaction->ST s ()
transact b (Transaction cFrom cTo amt) = do
  modifySTRef' (b ! cFrom) (subtract amt)
  modifySTRef' (b ! cTo) (+ amt)

readLedger::Bank s->ST s Ledger
readLedger b = do
  let (n,v) = unzip $ toList b
  as <- mapM readSTRef v
  return $ zip n as

