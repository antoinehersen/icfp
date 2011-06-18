type Id = String


--- from http://itscstime.blogspot.com/2011/04/learning-lambda-calculus-lambda-ski.html

data SKI = S | K | I | App SKI SKI | Var Id deriving (Show, Eq)
data Lambda = LVar Id | LApp Lambda Lambda | Abs Id Lambda deriving (Show, Eq)

-- list free variable
fv (Var id) = [id]
fv (App e1 e2) = fv e1 ++ fv e2
fv _ = []

freeIn id term = id `elem` fv term

lambdaToSKI :: Lambda -> SKI
lambdaToSKI (LVar id) = Var id
lambdaToSKI (LApp e1 e2) = App (lambdaToSKI e1) (lambdaToSKI e2)
lambdaToSKI (Abs id term) = lamb id $ lambdaToSKI term

lamb id term | not (id `freeIn` term) = App K term
lamb id (Var id') | id == id' = I
lamb id (App e1 e2) = App (App S (lamb id e1)) (lamb id e1)