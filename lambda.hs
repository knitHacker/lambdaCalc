

data Expression = Var Char
                | Function Abstraction
                | Application Apply


instance Show Expression where
    show (Var c) = [c]
    show (Function f) = '(' : (show f) ++ ")"
    show (Application a) = (show a)

data Abstraction = Abstraction Char Expression

instance Show Abstraction where
    show (Abstraction c e) = '\\' : c : '.' : (show e)

data Apply = Apply Apply Expression
           | First Expression Expression

instance Show Apply where
    show (Apply a e) = '(' : (show a) ++ ")(" ++ (show e) ++ ")"
    show (First v1@(Var _) v2@(Var _)) = (show v1) ++ (show v2)
    show (First e1 e2) = '(' : (show e1) ++ ")(" ++ (show e2) ++")"

alphaReduction :: Abstraction -> Char -> Abstraction
alphaReduction (Abstraction old e) new = Abstraction new (alphaReduction' e)
    where
        alphaReduction' :: Expression -> Expression
        alphaReduction' (Var c) = if c == old then Var new else Var c
        alphaReduction' (Function (Abstraction c e)) = if c == old || c == new
                                                       then error
                                                            "Namespace clash. Not sure how to continue"
                                                       else Function (Abstraction c (alphaReduction' e))
        alphaReduction' (Application a) = Application (alphaReduction'' a)
            where
                alphaReduction'' :: Apply -> Apply
                alphaReduction'' (Apply a e) = Apply (alphaReduction'' a) (alphaReduction' e)
                alphaReduction'' (First e1 e2) = First (alphaReduction' e1) (alphaReduction' e2)

betaReduction :: Abstraction -> Expression -> Expression
betaReduction (Abstraction find e) replace = (betaReduction' e)
    where
        betaReduction' :: Expression -> Expression
        betaReduction' (Var c) = if c == find then replace else Var c
        betaReduction' (Function (Abstraction c e)) = if c == find
                                                       then error
                                                            "Namespace clash. Not sure how to continue"
                                                       else Function (Abstraction c (betaReduction' e))
        betaReduction' (Application a) = Application (betaReduction'' a)
            where
                betaReduction'' :: Apply -> Apply
                betaReduction'' (Apply a e) = Apply (betaReduction'' a) (betaReduction' e)
                betaReduction'' (First e1 e2) = First (betaReduction' e1) (betaReduction' e2)

applyOne :: Apply -> Expression
applyOne (First e1 e2) = applyExpOne e1 e2
applyOne (Apply a e) = Application (applyOne' a e)
    where
        applyOne' :: Apply -> Expression -> Apply
        applyOne' (First e1 e2) e3 = First (applyExpOne e1 e2) e3
        applyOne' (Apply a e) e2 = Apply (applyOne' a e) e2

applyAll :: Apply -> Expression
applyAll (First e1 e2) = applyExp e1 e2
applyAll (Apply a e) = applyExp (applyAll a) e

applyExpOne :: Expression -> Expression -> Expression
applyExpOne v@(Var _) e2 = Application (First v e2)
applyExpOne (Application a) e2 = Application (Apply a e2)
applyExpOne (Function a) e2 = (betaReduction a e2)

applyExp :: Expression -> Expression -> Expression
applyExp v@(Var _) e2 = Application (First v (reduce e2))
applyExp (Application a) e2 = Application (Apply a e2)
applyExp (Function a) e2 = reduce (betaReduction a e2)

apply :: String -> String -> Expression
apply e1 e2 = applyExp (lambda e1) (lambda e2)

applyMix :: Expression -> String -> Expression
applyMix e1 e2 = applyExp e1 (lambda e2)

reduce :: Expression -> Expression
reduce v@(Var _) = v
reduce (Function (Abstraction c e)) = Function (Abstraction c (reduce e))
reduce (Application a) = applyAll a

reduceOne :: Expression -> Expression
reduceOne v@(Var _) = v
reduceOne (Function (Abstraction c e)) = Function (Abstraction c (reduceOne e))
reduceOne (Application a) = applyOne a

data ParseTree = Paren ParseTree ParseTree
               | Lambda ParseTree ParseTree
               | App ParseTree ParseTree
               | Const Char
               | End deriving (Show, Eq)


parse :: String -> ParseTree
parse l = let (tree, rmd) = parse' l in
            case rmd of [] -> tree
                        _ -> error "Something didn't parse right"
    where
        parse' :: String -> (ParseTree, String)
        parse' [] = error "Syntax error"
        parse' ('(' : tl)  = let (paren,rest) = parse' tl in
                                case rest of [] -> error ("Missing end paren: "++tl++" -> \n"++(show paren))
                                             (')':[]) -> (Paren paren End, [])
                                             (')':nt) -> let (more, rest2) = parse' nt in
                                                    (Paren paren more, rest2)
                                             _ -> error ("Paren: No case for " ++ rest)

        parse' ('\\' : tl) = let (param, rest) = parse' tl in
                                case rest of [] -> error "No body for function"
                                             ('.':nt) -> let (body, rest2) = parse' nt in
                                                        (Lambda param body, rest2)
                                             _ -> error ("Body: No case for " ++ rest)
        parse' s@(')':_) = (End, s)
        parse' (c : []) = (Const c, [])
        parse' (c : s@('.' : _)) = (Const c, s)
        parse' (c : s@(')' : _)) = (Const c, s)
--        parse' (c : s@('(' : tl)) = (Const c, s)
        parse' (c : tl) = let (exp, rest) = parse' tl in
                            (App (Const c) exp, rest)


compile :: ParseTree -> Expression
compile (Paren f End) = compile f
compile (Paren f b) = Application (compileToLeft f b)
compile (Lambda (Const c) b) = Function (Abstraction c (compile b))
compile (Lambda (App (Const c) r) b) =  Function (Abstraction c (compile (Lambda r b)))
compile (Lambda _ _) = error "Only constants can be parameters"
compile (App l End) = compile l
compile (App l r) = Application (compileToLeft l r)
compile (Const c) = Var c
compile End = error "Program must contain something"


compileToLeft :: ParseTree -> ParseTree -> Apply
compileToLeft p1 p2 = let exps = (reverseApp' p2) in
                        foldl Apply (First (compile p1) (head exps)) (tail exps)
    where
        reverseApp' :: ParseTree -> [Expression]
        reverseApp' (Paren f End) = [compile f]
        reverseApp' (App f End) = [compile f]
        reverseApp' (Paren f b) = (compile f) : (reverseApp' b)
        reverseApp' (App f b) = (compile f) : (reverseApp' b)
        reverseApp' p = [compile p]

lambda :: String -> Expression
lambda = compile . parse

