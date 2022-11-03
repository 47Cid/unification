module Lib
    ( run
    ) where
import qualified Data.Map   as M
import           Data.Maybe (fromMaybe)
import Debug.Trace (trace)

run :: IO ()
run = print $ unify (Application "f" [Variable "X", Application "h" [Variable "X"], Variable "Y", Application "g" [Variable "Y"]])
                    (Application "f" [Application "g" [Variable "Z"], Variable "W", Variable "Z", Variable "X"]) (Binding M.empty)
-- run = print $ unify (Application "f" [Atom "t"])
--                     (Variable "X") (Binding M.empty)

data ProgVal = Atom String
             | Variable String
             | Application String [ProgVal]
      deriving(Show,Read,Eq)

data Binding  =   Binding (M.Map String String) | Error String
      deriving(Show,Read,Eq)

unify :: ProgVal -> ProgVal -> Binding -> Either Binding String
unify _ _ (Error err)          =  Right err
unify (Variable x) y bindings  = unifyVariable (Variable x) y bindings
unify  x (Variable y) bindings = unifyVariable (Variable y) x bindings
unify (Application xname [x]) (Application yname [y]) bindings = trace (xname ++ yname) unify x y bindings
unify (Application xname (x:xs)) (Application yname (y:ys)) bindings = unify (Application xname xs) (Application yname ys)
                                                 $ either id (\_ -> Error "Application Fail") (unify x y bindings)
unify _ _ _ = Right "Incorrect format!!!"


unifyVariable :: ProgVal -> ProgVal -> Binding -> Either Binding String
unifyVariable (Variable y) x  (Binding bindings)
                                                | var /= "" = unify (Variable var) x (Binding bindings)
                                                 where var = fromMaybe "" $ M.lookup y bindings
unifyVariable (Variable y) (Variable x)  (Binding bindings)
                                                | var /= "" = unify (Variable y) (Variable var) (Binding bindings)
                                                 where var = fromMaybe "" $ M.lookup x bindings
unifyVariable (Variable y) (Atom x) (Binding bindings)
                                                | occursCheck (Variable x) (Atom y) (Binding bindings)  =  Right "occursCheck is true"
                                                | otherwise =Left (Binding $ M.insert y x bindings)
unifyVariable (Variable y) (Variable x) (Binding bindings)
                                                | occursCheck (Variable y) (Variable x) (Binding bindings)  =  Right "occursCheck is true"
                                                | otherwise =Left (Binding $ M.insert y x bindings)
unifyVariable (Variable y) (Application name args) (Binding bindings)
                                                | occursCheck (Variable y) (Application name args) (Binding bindings)  =  Right "occursCheck is true"
                                                | otherwise =Left (Binding $ M.insert y  (name ++ show args) bindings)
unifyVariable _ _ _ = Right "Error Unifying Variable"


occursCheck :: ProgVal -> ProgVal -> Binding -> Bool
occursCheck (Variable v) (Variable x) (Binding bindings)
                 | Variable v == Variable x = True
                 | var /= ""= occursCheck (Variable v) (Variable var) (Binding bindings)
                  where var = fromMaybe "" $ M.lookup x bindings
occursCheck (Variable v) (Application _ [Variable x]) bindings = occursCheck (Variable v) (Variable x) bindings
occursCheck v (Application name (x:xs)) bindings = occursCheck v x bindings || occursCheck v (Application name xs) bindings
occursCheck _ _ _ = False
