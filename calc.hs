data Expr a = Var String
              | Const a 
              | Add (Expr a) (Expr a)
              | Mul (Expr a) (Expr a)
              | Div (Expr a) (Expr a)
              | Pow (Expr a) Integer
              | Apply Func (Expr a)
    deriving (Eq)

data Func = Sin | Cos | Log | Exp 
-- | Asin | Acos | Atan
    deriving (Eq)

instance Show a => Show (Expr a) where
  show :: Expr a -> String
  show (Var a)   = a
  show (Const a) = show a 
  show (Add a b) = show a ++ " + " ++ show b
  show (Mul p1 p2)
    | (Mul x y) <- p1, (Mul a b) <- p2 = show p1 ++ show p2
    | (Mul x y) <- p1 = show p1 ++ "(" ++ show p2 ++ ")"
    | (Mul x y) <- p2 = "(" ++ show p1 ++ ")" ++ show p2
    | otherwise = "(" ++ show p1 ++ ")" ++ "(" ++ show p2 ++ ")"
  show (Div a b) = show a ++ " / " ++ show b
  show (Pow a b) = show a ++ " ^ " ++ show b

eval :: Floating a => Expr a -> a -> a
eval (Var a)   c         = c
eval (Const a) c         = a
eval (Add f g) c         = eval f c + eval g c
eval (Mul f g) c         = eval f c * eval g c
eval (Div f g) c         = eval f c / eval g c
eval (Pow f g) c = eval f c ** fromInteger g
eval (Apply func expr) c = 
  case func of
    Sin -> sin (eval expr c)
    Cos -> cos (eval expr c)
    Log -> log (eval expr c)
    Exp -> exp (eval expr c)

diff :: Fractional a => Expr a -> Expr a
diff (Var a)   = Const 1 
diff (Const _) = Const 0
diff (Add f g) = Add (diff f) (diff g)
diff (Mul f g) = Add (Mul (diff f) g) (Mul f (diff g)) -- Product Rule
diff (Div f g) = Div (Add (Mul g $ diff f) $ Mul (Const (-1)) (Mul f $ diff g)) (Pow g 2) -- Quotient Rule
diff (Pow f g) = Mul (diff f) (Mul (Const $ fromInteger g) (Pow f (fromInteger g-1))) -- Chain rule
diff (Apply func expr) = diffFunc (Apply func expr)


diffFunc :: Fractional a => Expr a -> Expr a
diffFunc (Apply Sin expr) = Mul (diff expr) (Apply Cos expr)
diffFunc (Apply Cos expr) = Mul (diff expr) $ Mul (Const (-1)) (Apply Sin expr)
diffFunc (Apply Log expr) = Div (diff expr) expr 
diffFunc (Apply Exp expr) = Mul (diff expr) (Apply Exp expr)

