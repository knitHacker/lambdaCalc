
import Debug.Trace

{-
data Term a where
    Lit :: Int -> Term Int
    Succ :: Term Int -> Term Int
    IsZero :: Term Int -> Term Bool
    If :: Term Bool -> Term a -> Term a -> Term a
    Pair :: Term a -> Term b -> Term (a,b)
    Func :: 

eval :: Term a -> a
eval (Lit i) = i
eval (Succ t) = 1 + eval t 
eval (IsZero t) = eval t == 0
eval (If b e1 e2) = if eval b then eval e1 else eval e2
eval (Pair a b) = (eval a, eval b)


add :: Term a -> Term a -> Term a
add (Lit i) = yes

-}


data Exp where
    Name :: String -> Exp
    Func :: String -> Exp -> Exp
    Appl :: Exp -> Exp -> Exp

func :: [String] -> Exp -> Exp
func [] e = e 
func (h:tl) e = (Func h (func tl e))

appl :: [Exp] -> Exp
appl (h:[]) = h
appl (h:tl) = Appl h (appl tl)

i = Func "x" (Name "x")
test = Appl (Func "x" (Func "y" (Appl (Name "x") (Name "y")))) (Name "y")

substituteS :: String -> String -> Exp -> Exp
substituteS old new (Name x) = if x == old then Name new else Name x
substituteS old new (Func x e) = if x == old then substituteS old new e else (Func x e)
substituteS old new e@(Appl e1 e2) = Appl (substituteS old new e1) (substituteS old new e2)

evalS :: Exp -> String
evalS (Name s) = s 
evalS (Func s e) = '\\':s++"."++(evalS e)
evalS (Appl (Func s fe) e2) = evalS (substituteS s (evalS e2) fe)
evalS (Appl e1 e2) = (evalS e1)++(evalS e2) 

substitute :: String -> Exp -> Exp -> Exp 
substitute old new (Name x) = if x == old then new else Name x
substitute old new (Func x e) = Func x (substitute old new e)
substitute old new (Appl e1 e2) = Appl (substitute old new e1) (substitute old new e2)

eval :: Exp -> Exp 
eval e@(Name _) = e
eval (Func s e) = (Func s (eval e))
eval (Appl (Func s fe) e2) = (eval (substitute s (eval e2) fe))
eval (Appl e1@(Name _) e2) = (Appl e1 e2)
eval (Appl e1 e2) = case eval e1 of 
    Func s fe -> (substitute s (eval e2) fe)
    e -> (Appl e (eval e2))

instance Show Exp where
    show (Name x) = x
    show (Func x e) = '\\':x++".("++(show e)++")"
    show (Appl e1 e2) = "("++(show e1)++")"++"("++(show e2)++")"

-- \sz.z == \s.\z.z
zero = func ["s","z"] (Name "z")
-- \sz.s(z) == \s.s(\z.z)
one = func ["s", "z"] (appl [Name "s", Name "z"])

-- \wyx.y(wyx) == 
succ = func ["w", "y", "x"] (appl [Name "y", Name "w", Name "y", Name "x"])




