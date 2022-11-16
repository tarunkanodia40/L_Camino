import Parser
import Data.Set

-- <--------------- Q2 --------------->
-- Works as findFreeVars "(\\x.x y)" => ["y"]

freeVars :: Term -> Set Variable
freeVars (Var x) = singleton x
freeVars (App t1 t2) = freeVars t1 `union` freeVars t2
freeVars (Abs x t) = delete x (freeVars t)

findFreeVars :: String -> Set Variable
findFreeVars s = freeVars x where x=parse s



-- <-------------- Q3 --------------->

subst :: Variable -> Term -> Term -> Term
subst x (Var y) t = case x == y of
  True -> t
  False -> Var y
subst x (App s1 s2) t = App (subst x s1 t) (subst x s2 t)
subst x (Abs y s) t
  | x == y = Abs y s
  | y `notMember` freeVars t = Abs y (subst x s t)
  | otherwise = let z = fresh (freeVars t `union` freeVars s) in
    Abs z (subst x (subst y s (Var z)) t)


-- <------------- Q4 --------------->

-- Works As betaReduce "(\\x.x y)(\\a.(\\b. b a c))"  => \b.b y c
eval expr = case expr of
  App e1 e2 -> ( case eval e1 of
    Abs x e -> eval (subst x e e2);
    App e1a e1b -> App e1 e2;
    Var x -> App e1 e2; );
  Var x -> Var x;
  Abs x e -> Abs x e;

betaReduce :: String -> Term
betaReduce s = eval x where x = parse s

instance Eq Term where
  (==) (Var x) (Var y) = x == y
  (==) (App t1 t2) (App s1 s2) = (t1 == s1) && (t2 == s2)
  (==) (Abs x t) (Abs y s) = let z = fresh (freeVars t `union` freeVars s) in
    (subst x t (Var z)) == (subst y s (Var z))
  (==) _ _ = False

instance Show Term where
  show = showAbs
    where showAtom (Var x) = x
          showAtom t = "(" ++ show t ++ ")"

          showAbs (Abs x t) = "( \\" ++ x ++ "." ++ showAbs t ++ " )"
          showAbs t = showApp t

          showApp (App t1 t2) = showApp t1 ++ " " ++ showAtom t2
          showApp t = showAtom t
  
