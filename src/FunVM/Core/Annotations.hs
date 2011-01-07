{-# LANGUAGE DeriveFunctor, GADTs, FlexibleContexts, StandaloneDeriving, UndecidableInstances #-}

data ExprF r
  = ValF Int
  | AddF r r
  deriving (Functor, Show)

newtype Fix f = In { out :: f (Fix f) }

--deriving instance Show (f (Fix f)) => Show (Fix f)
instance Show (f (Fix f)) => Show (Fix f) where
  show x = "{" ++ show (out x) ++ "}"

data Ann x f a = Ann x (f a)
  --deriving Show

instance (Show x, Show (f a)) => Show (Ann x f a) where
  show (Ann x y) = show x ++ "_" ++ show y

type ExprSize = Fix (Ann Size ExprF)

type Size = Int

test :: Fix ExprF
test = addf (addf (valf 3) (valf 2)) (valf 1)

size :: ExprF Size -> Size
size (ValF _) = 1
size (AddF x y) = 1 + x + y

eval :: ExprF Int -> Int
eval (ValF n) = n
eval (AddF x y) = x + y

(&) :: Functor f => (f a -> a) -> (f b -> b) -> f (a, b) -> (a, b)
f & g = undefined  -- fmap $ \(x,y) -> (f x, g y)


with :: Functor f => (f x -> x) -> Fix f -> Fix (Ann x f)
with alg (In x) = In (Ann (alg (fmap (\(In (Ann y _)) -> y) xxx)) xxx)
  where
    xxx = fmap (with alg) x

valf :: Int -> Fix ExprF
valf x = In $ ValF x

addf :: Fix ExprF -> Fix ExprF -> Fix ExprF
addf a b = In $ AddF a b
