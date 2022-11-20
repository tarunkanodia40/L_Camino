import Parser
import Data.Set
-- <--------------- Q1 --------------->
--Check the required lambda term as 
-- parse (Lambda_term)
--If there is no exception or error, then the term is a valid term, else it is not

-- <--------------- Q2 --------------->
-- Works as findFreeVars "(\\x.x y)" => ["y"]

freeVars :: Term -> Set Variable
-- FV[x] = {x}
freeVars (Var x) = singleton x
-- FV[M N] = FV[M] union FV[N]
freeVars (App t1 t2) = freeVars t1 `union` freeVars t2
-- FV[\x.M] = FV[M] \ {x}
freeVars (Abs x t) = if member x (freeVars t) == True then
                        delete x (freeVars t)
                     else
                        freeVars t

findFreeVars :: String -> Set Variable
findFreeVars s = freeVars (parse s)


-- <-------------- Q3 --------------->
-- Works as substi "(\\y.x)" "x" "z" which is equal to (\y.x) [x:=z]
-- The second argument should be just a variable whereas the first and last arguements are lamda terms (all in form of strings)

subst :: Variable -> Term -> Term -> Term
subst x (Var y) t = if x == y 
                      then t -- x[x:= N] = N
                    else Var y  -- y[x:= N] = y
subst x (App s1 s2) t = App (subst x s1 t) (subst x s2 t) -- MP[x:=N] = M[x:=N] P[x:=N]
subst x (Abs y s) t = if x == y  
                        then Abs y s -- (\x.M)[x:= N] = (\x.M) as x is not free in the lambda term
                      else if y `notMember` freeVars t  
                        then Abs y (subst x s t) --(\y.M)[x:= N] = (\y.M[x:=N]) in case if y is not free in the lambda term N
                      else let z = fresh (freeVars t `union` freeVars s) in 
                            Abs z (subst x (subst y s (Var z)) t) -- Case when y is free in the lambda term, we need alpha renaming of the original term

substi :: String -> Variable -> String -> Term
substi x y z = (subst y (parse x) (parse z)) 

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
betaReduce s = eval (parse s)

-- xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx 

instance Eq Term where
  (==) (Var x) (Var y) = x == y
  (==) (App t1 t2) (App s1 s2) = (t1 == s1) && (t2 == s2)
  (==) (Abs x t) (Abs y s) = let z = fresh (freeVars t `union` freeVars s) in
    (subst x t (Var z)) == (subst y s (Var z))
  (==) _ _ = False

-- Printing Lambda terms
showAtom (Var x) = x
showAtom t = "(" ++ show t ++ ")"

showAbs (Abs x t) = "( \\" ++ x ++ "." ++ showAbs t ++ " )"
showAbs t = showApp t

showApp (App t1 t2) = showApp t1 ++ " " ++ showAtom t2
showApp t = showAtom t

instance Show Term where
  show = showAbs

  
