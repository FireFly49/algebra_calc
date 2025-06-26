

data Expr a =   Var
                | Const a
                | Add (Expr a) (Expr a)
                | Mul (Expr a) (Expr a)
                | Div (Expr a) (Expr a)
                | Apply Func (Expr a)
  deriving (Eq)

data Func = Sin | Cos | Log | Exp 
-- | Asin | Acos | Atan
    deriving (Eq)


instance Show a => Show (Expr a) where
  show :: (Expr a) -> String 
  show Var = "x"
  show (Const a) = show a
  show (Add p1 p2) = show p1 ++ " + " ++ show p2
  show (Mul p1 p2)
    | (Mul x y) <- p1, (Mul a b) <- p2 = show p1 ++ show p2
    | (Mul x y) <- p1 = show p1 ++ "(" ++ show p2 ++ ")"
    | (Mul x y) <- p2 = "(" ++ show p1 ++ ")" ++ show p2
    | otherwise = "(" ++ show p1 ++ ")" ++ "(" ++ show p2 ++ ")"
  show (Div p1 p2) = show p1 ++ " / " ++ show p2

eval :: Floating a => Expr a -> a -> a
eval Var       c = c
eval (Const a) c = a
eval (Add f g) c = eval f c + eval g c
eval (Mul f g) c = eval f c * eval g c
eval (Div f g) c = eval f c / eval g c
eval (Apply func expr) c = 
  case func of
    Sin -> sin (eval expr c)
    Cos -> cos (eval expr c)
    Log -> log (eval expr c)
    Exp -> exp (eval expr c)

neg :: Fractional a => Expr a -> Expr a
neg = Mul (Const (-1))

diff :: Fractional a => Expr a -> Expr a
diff Var               = Const 1
diff (Const a)         = Const 0
diff (Add f g)         = Add (diff f) (diff g)
diff (Mul f g)         = Add (Mul f $ diff g) (Mul g $ diff f)
diff (Div f g)         = Div (Add (Mul g $ diff f) $ neg (Mul f $ diff g)) (Mul g g)
diff (Apply func expr) = diffFunc (Apply func expr)

diffFunc :: Fractional a => Expr a -> Expr a
diffFunc (Apply Sin expr) = Mul (diff expr) (Apply Cos expr)
diffFunc (Apply Cos expr) = Mul (diff expr) $ neg (Apply Sin expr)
diffFunc (Apply Log expr) = Div (diff expr) expr 
diffFunc (Apply Exp expr) = Mul (diff expr) (Apply Exp expr)

