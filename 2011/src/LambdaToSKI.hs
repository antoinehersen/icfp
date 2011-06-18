import Test.HUnit

type Id = String
-- from by http://itscstime.blogspot.com/2011/04/learning-lambda-calculus-lambda-ski.html

-- Using wikipedia article:

-- 1/ T[x] => x
-- 2/ T[(E₁ E₂)] => (T[E₁] T[E₂])
-- 3/ T[λx.E] => (K T[E]) (if x does not occur free in E)
-- 4/ T[λx.x] => I
-- 5/ T[λx.λy.E] => T[λx.T[λy.E]] (if x occurs free in E)
-- 6/ T[λx.(E₁ E₂)] => (S T[λx.E₁] T[λx.E₂])


data SKI = S | K | I | App SKI SKI | Var Id deriving (Show, Eq)
data Lambda = LVar Id | LApp Lambda Lambda | Abs Id Lambda deriving (Show, Eq)

-- instance Show SKI where
pshow S = "S"
pshow K = "K"
pshow I = "I"
pshow (App a b) = "(" ++ show a ++ show b ++ ")"
pshow (Var i ) = i

-- list free variable
fv (Var id) = [id]
fv (App e1 e2) = fv e1 ++ fv e2
fv _ = []

freeIn id term = id `elem` fv term

lambda2ski :: Lambda -> SKI
lambda2ski (LVar id) = Var id
lambda2ski (LApp e1 e2) = App (lambda2ski e1) (lambda2ski e2)
lambda2ski (Abs id lterm) = lamb id $ lambda2ski lterm

lamb id term | not (id `freeIn` term) = App K term
lamb id (Var id') | id == id' = I
lamb id (App e1 e2) = App (App S (lamb id e1)) (lamb id e2)

-- Trying to do with the lamb is actually quite tricky because of rule number 5


test1 = Abs "x" ( Abs "y" (LApp (LVar "y" ) (LVar "x")))
-- ((S(K(SI)))((S(KK))I))

-- \abcd. bcda
test2 = Abs "a" ( Abs "b" ( Abs "c" (Abs "d" ( LApp (LApp ( LApp (LVar "d") (LVar "c")) (LVar "b")) (LVar "a")))))
-- ((S(K(S((S(KS))((S(K(S(KS))))((S(K(S((S(KS))((S(K(SI)))((S(KK))I))))))((S(KK))((S(KK))I))))))))((S(KK))((S(KK))((S(KK))I))))

tests = test [ "rule 1" ~:
               (Var "x") ~=? lambda2ski (LVar "x"),
               "wikipedia example" ~:
               (App (App S (App K (App S I))) (App (App S (App K K)) I)) ~=? lambda2ski ( Abs "x" ( Abs "y" (LApp (LVar "y" ) (LVar "x"))))
             ]