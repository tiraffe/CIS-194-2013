module Party where

import Employee
import Data.Monoid
import Prelude
import Data.Tree
import Data.List
import System.IO


instance Monoid GuestList where
    mempty = GL [] 0

instance Semigroup GuestList where
    (<>) (GL e1 f1) (GL e2 f2) = GL (e1 ++ e2) (f1 + f2)

glCons :: Employee -> GuestList -> GuestList
glCons e gl = gl <> GL [e] (empFun e)

treeFold :: (a -> [b] -> b) -> Tree a -> b
treeFold f tree = f (rootLabel tree) $ map (treeFold f) (subForest tree) 

nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel root costs   = (GL [root] (empFun root) <> withRootBest costs, notRootBest costs)
    where withRootBest = mconcat . map snd
          notRootBest  = mconcat . map (\(x, y) -> max x y)

maxFun :: Tree Employee -> GuestList
maxFun = (\(x, y) -> max x y) . treeFold nextLevel 

toString :: GuestList -> String 
toString (GL emps fun) = unlines $ ["Total Fun: " ++ show fun] ++ (sort $ map empName emps)

main = do
    contents <- readFile "company.txt"
    putStr . toString $ maxFun (read contents :: Tree Employee)
