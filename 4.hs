import Test.QuickCheck
import Data.Semigroup

data Three a b c = Three a b c deriving (Eq, Show)

instance (Semigroup a, Semigroup b, Semigroup c) => Semigroup (Three a b c) where
    (Three a b c) <> (Three a' b' c') = Three (a <> a') (b <> b') (c <> c')

instance (Semigroup a, Semigroup b, Semigroup c, Monoid a, Monoid b, Monoid c) => Monoid (Three a b c) where
    mempty = Three mempty mempty mempty
    mappend = (<>)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
    arbitrary = do
        a <- arbitrary 
        b <- arbitrary
        c <- arbitrary
        return $ Three a b c

semigroupAssoc :: (Eq a, Semigroup a) => a -> a -> a -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq a, Monoid a) => a -> Bool
monoidLeftIdentity a = mappend a mempty == a 

monoidRightIdentity :: (Eq a, Monoid a) => a -> Bool
monoidRightIdentity a = mappend mempty a == a 

x = 2 + 2


type ThreeStr = Three String String String
type SemiAssoc = ThreeStr -> ThreeStr -> ThreeStr -> Bool

main :: IO ()
main = do 
    quickCheck (semigroupAssoc :: SemiAssoc)
    quickCheck (monoidLeftIdentity :: ThreeStr -> Bool)
    quickCheck (monoidRightIdentity :: ThreeStr -> Bool) 