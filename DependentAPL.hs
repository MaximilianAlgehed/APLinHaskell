{-# LANGUAGE
 GADTs,
 DataKinds,
 KindSignatures,
 MultiParamTypeClasses,
 FunctionalDependencies,
 FlexibleInstances,
 FlexibleContexts,
 TypeFamilies,
 TypeOperators,
 UndecidableInstances,
 AllowAmbiguousTypes,
 PolyKinds 
 #-}

data Nat = Zero | Succ Nat

data Dimensionality = Unit | Dimensions [Dimensionality]

class Dimension a (b :: Dimensionality) | a -> b

instance Dimension Nat Unit

instance Dimension (APLValue d a) d

data APLValue (b :: Dimensionality) (a :: a2) where
    Nil  :: APLValue (Dimensions '[]) '[]
    Cons :: (Dimension a d) => a -> APLValue (Dimensions ds) bs -> APLValue (Dimensions (d ': ds)) (a ': bs)

class GetSingleton a b

instance GetSingleton (n :: Nat) n

instance (GetSingleton a b) => GetSingleton (APLValue bs '[a]) b

class SameLength (bs :: [a]) (lst :: [b])

instance SameLength '[] '[] 

instance (SameLength as bs) => SameLength (a ': as) (b ': bs)

class Units (as :: [a]) (bs :: Dimensionality)

instance Units '[] (Dimensions '[])

instance (Units as (Dimensions units)) => Units (a ': as) (Dimensions (Unit ': units))

class Iota i (xs :: [a]) where
    iota :: (Units xs ys) => APLValue ys xs

instance Iota Zero '[] where
    iota = Nil

instance (GetSingleton a b, Iota b xs) => Iota a xs 
