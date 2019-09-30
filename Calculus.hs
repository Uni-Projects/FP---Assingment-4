-- Exercise week 4
-- Johan Urban s1024726
-- Paolo Scattolin s1023775
module Calculus
where

data Primitive
  =  Sin  -- trigonometric: sine
  |  Cos  -- cosine
  |  Exp  -- exponential
  deriving (Show, Eq)


infixl 6 :+:
infixl 7 :*:
infixr 9 :.:

data Function
  =  Const Rational         -- constant function
  |  Id                     -- identity
  |  Prim Primitive         -- primitive function
  |  Function :+: Function  -- addition of functions
  |  Function :*: Function  -- multiplication of functions
  |  Function :.: Function  -- composition of functions
  deriving (Show, Eq)

apply :: Function -> (Double -> Double)
apply (Const c) val   = fromRational c
apply (Id) val        = val
apply (Prim Sin) val  = sin val
apply (Prim Cos) val  = cos val
apply (Prim Exp) val  = exp val
apply (f1 :+: f2) val = (apply (f1) val) + (apply (f2) val)
apply (f1 :*: f2) val = (apply (f1) val) * (apply (f2) val)
apply (f1 :.: f2) val = apply f1(apply f2 val)

derive   :: Function -> Function
derive (Const c)      = Const 0
derive (Id)           = Const 1
derive (Prim Sin)     = Prim Cos
derive (Prim Cos)     = Const (-1) :*: Prim Sin
derive (Prim Exp)     = Prim Exp
derive (f1 :+: f2)    = (derive f1) :+: (derive f2)
derive (f1 :*: f2)    = ((derive f1) :*: f2) :+: ((derive f2):*: f1)
derive (f1 :.: f2)    = (((derive f1) :.: f2) :*: (derive f2))

simplify :: Function -> Function   --calls the auxiliary function simplify'
simplify f = simplify' f (Const 0)

simplify' :: Function -> Function -> Function --repeats until the result of simplify' is the same
simplify' this last
   | this == last = this
   | otherwise    = simplify' new this
                       where new = simplify'' this

simplify'' :: Function -> Function
simplify'' (Const a :+: Const b) = Const (a + b)
simplify'' (Const a :*: Const b) = Const (a * b)
simplify'' (f :*: Const 1)       = simplify f
simplify'' (Const 1 :*: f)       = simplify f
simplify'' (f :+: Const 0)       = simplify f
simplify'' (Const 0 :+: f)       = simplify f
simplify'' (f :*: Const 0)       = Const 0
simplify'' (Const 0 :*: f)       = Const 0
simplify'' (f1 :*: f2)           = (simplify f1) :*: (simplify f2)
simplify'' (f1 :+: f2)           = (simplify f1) :+: (simplify f2)
simplify'' f                     = f
