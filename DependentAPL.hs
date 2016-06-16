{-# LANGUAGE
 GADTs,
 DataKinds,
 KindSignatures,
 MultiParamTypeClasses,
 FunctionalDependencies,
 FlexibleInstances,
 TypeOperators
 #-}

data Dimensionality = Unit | Dimensions [Dimensionality]

class Dimension a (b :: Dimensionality) | a -> b

instance Dimension Int Unit

instance Dimension () Unit

-- The absolutely lovely type of an APL list
data APLValue (b :: Dimensionality) a where
    Nil  :: APLValue (Dimensions '[]) '[]
    Cons :: (Dimension a d) => a -> APLValue (Dimensions ds) bs -> APLValue (Dimensions (d ': ds)) (a ': bs)

instance Dimension (APLValue d a) d
