data Fraction a = Var
                | Const a
                | Add (Fraction a) (Fraction a)
                | Mul (Fraction a) (Fraction a)
                | Div (Fraction a) (Fraction a)
  deriving (Show, Eq)


eval :: Fractional a => Fraction a -> a -> a
eval Var       c = c
eval (Const a) c = a
eval (Add f g) c = eval f c + eval g c
eval (Mul f g) c = eval f c * eval f c
eval (Div f g) c = eval f c / eval f c

neg :: Fractional a => Fraction a -> Fraction a
neg = Mul (Const (-1))

diff :: Fractional a => Fraction a -> Fraction a
diff Var = Const 1
diff (Const a) = Const 0
diff (Add f g) = Add (diff f) (diff g)
diff (Mul f g) = Add (Mul f $ diff g) (Mul g $ diff f)
diff (Div f g) = Div (Add (Mul g $ diff f) $ neg (Mul f $ diff g)) (Mul g g)
