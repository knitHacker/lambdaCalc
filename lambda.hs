import Data.List
import Data.Char

type ID = String

data Name = Name ID deriving(Show, Eq)

data Expr = Ident Name | Func Name Expr | Appl Expr Expr deriving(Show, Eq) 

{-
In order to avoid cluttering expressions with parenthesis, we adopt the
convention that function application associates from the left, that is, the expression
is evaluated applying the expressions as follows:
(...((E_1E_2)E_3)...E_n)
-}

inExpr :: Name -> Expr -> Bool
inExpr n1 (Ident n2) = n1 == n2
inExpr n1 (Func n2 exp) = n1 /= n2 && (inExpr n1 exp) && not (inExpr n2 exp)
inExpr n1 (Appl e1 e2) = (inExpr n1 e1) || (inExpr n1 e2)

bound :: Expr -> [Name] -> [Name]
bound (Ident n) looking = if elem n looking then [n] else []
bound (Func n exp) looking = bound exp (nub (n:looking)) 
bound (Appl e1 e2) looking = nub ((bound e1 looking)++(bound e2 looking))

free :: Expr -> [Name] -> [Name]
free (Ident n) looking = if notElem n looking then [n] else []
free (Func n exp) looking = free exp (nub (n:looking))
free (Appl e1 e2) looking = nub ((free e1 looking)++(free e2 looking))

identyFunction = Func (Name "x") (Ident (Name "x"))


canReduce :: Expr -> Bool
canReduce (Ident _) = False
canReduce (Func _ exp) = canReduce exp
canReduce (Appl e1 e2) = undefined

reduce :: Expr -> Expr
reduce = undefined

returnNext :: Char -> Char
returnNext 'z' = 'a'
returnNext c = chr (ord c + 1)

updateName :: Name -> [Name] -> Name
updateName n@(Name s) taken = if elem n taken 
                        then let new_last = returnNext (last s) in
                            if new_last == 'a'
                                then updateName (Name (s++"a")) taken
                                else updateName (Name ((init s)++[new_last])) taken
                        else n

scrapeNames :: Expr -> [Name]
scrapeNames (Ident n) = [n]
scrapeNames (Func n exp) = n:(scrapeNames exp)
scrapeNames (Appl e1 e2) = (scrapeNames e1) ++ (scrapeNames e2)

uniquify :: Expr -> [Name] -> Expr 
uniquify (Ident n) taken = (Ident (updateName n taken))
uniquify (Func n exp) taken = let new_name = updateName n taken in
                            let new_exp = if n /= new_name 
                                            then replace exp n new_name
                                            else exp in
                            (Func new_name (uniquify exp taken))
uniquify (Appl e1 e2) taken = let new_e1 = uniquify e1 taken in
                                let e1_ids = scrapeNames new_e1 in 
                                    let new_e2 = uniquify e2 (taken++e1_ids) in 
                                    (Appl new_e1 new_e2)

replace :: Expr -> Name -> Name -> Expr
replace (Ident n) old new = if n == old
                                then (Ident new) 
                                else (Ident n)
replace (Func n exp) old new = let new_exp = replace exp old new in
                                if n == old
                                  then (Func new new_exp)
                                  else (Func n new_exp)
replace (Appl e1 e2) old new = Appl (replace e1 old new) (replace e2 old new)

eval :: Expr -> Expr
eval exp = if canReduce exp then eval (reduce exp) else exp
