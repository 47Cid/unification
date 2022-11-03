module Unification
    (
      ProgVal(..)       
     ,Binding(..)
     ,unify
     ,run
    ) where
import qualified Data.Map    as M
import           Data.Maybe  (fromMaybe)

run :: IO ()
run = print $ unify (Application "f" [Atom "a", Variable "V" , Application "bar" [Variable "D"]])
                    (Application "f" [Variable "D",  Atom "k", Application "bar" [Atom "a"]]) (Binding M.empty)

data ProgVal = Atom String
             | Variable String
             | Application String [ProgVal]
      deriving(Show, Eq, Ord)

data Binding  =   Binding (M.Map ProgVal ProgVal) | Error String
      deriving(Show, Eq)

unify :: ProgVal -> ProgVal -> Binding -> Either Binding String
unify _ _ (Error err)          =  Right err
unify (Variable x) y bindings  =  unifyVariable (Variable x) y bindings
unify  x (Variable y) bindings =  unifyVariable (Variable y) x bindings
unify (Application _ [x]) (Application _ [y]) bindings = unify x y bindings
unify (Application xname (x:xs)) (Application yname (y:ys)) bindings = unify (Application xname xs) (Application yname ys)
                                                 $ either id (\_ -> Error "Application Fail") (unify x y bindings)
unify _ _ bindings = Left bindings


unifyVariable :: ProgVal -> ProgVal -> Binding -> Either Binding String
unifyVariable (Variable y) x  (Binding bindings)
                                                | var /= Atom "" = unify var x (Binding bindings)
                                                 where var = fromMaybe (Atom "") $ M.lookup (Variable y) bindings
unifyVariable y (Variable x)  (Binding bindings)
                                                | var /= Atom "" = unify y var (Binding bindings)
                                                 where var = fromMaybe (Atom "") $ M.lookup (Variable x) bindings
unifyVariable y x bindings
                            | y /= x = extendBindings y x bindings
                            | otherwise = Left bindings


extendBindings :: ProgVal -> ProgVal -> Binding -> Either Binding String
extendBindings (Variable y) x (Binding bindings)
                            | occursCheck (Variable y) x (Binding bindings) = Right "Extend bindings failed"
                            | otherwise = Left (Binding $ M.insert (Variable y) x bindings)
extendBindings _ _ _ = Right "Inavlid binding format!"


occursCheck :: ProgVal -> ProgVal -> Binding -> Bool
occursCheck y x _
                 | y == x = True
occursCheck y (Variable x) (Binding bindings)
                 | var /= Atom "" = occursCheck y  var (Binding bindings)
                  where var = fromMaybe (Atom "") $ M.lookup (Variable x) bindings
occursCheck (Variable v) (Application _ [x]) bindings = occursCheck (Variable v) x bindings
occursCheck y (Application name (x:xs)) bindings = occursCheck y x bindings || occursCheck y (Application name xs) bindings
occursCheck _ _ _ = False
