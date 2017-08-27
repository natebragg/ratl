import Test.QuickCheck (infiniteListOf, vectorOf, shuffle, resize, generate, choose, elements, Gen(..))
import Test.QuickCheck.Modifiers (NonZero(..))
import Test.QuickCheck.Arbitrary (Arbitrary(..))

--import Randoms (problems)
import Data.Clp.StandardForm (StandardForm(..), solve)

notBothZero :: Gen [Double]
notBothZero = do
    a <- arbitrary
    NonZero b <- arbitrary
    shuffle [a, b]

instance Arbitrary StandardForm where
    arbitrary = StandardForm <$> ((,) <$> notBothZero <*> vectorOf 15 ((,) <$> notBothZero <*> arbitrary))

class Perturb a where
    perturb :: a -> Gen a

instance Perturb StandardForm where
    perturb (StandardForm (os, cs)) = do
        os' <- perturb os
        cs' <- perturb $ take 9000 $ concat $ repeat cs
        return $ StandardForm (os', cs')

instance Perturb Double where
    perturb n = do
        scale <- choose (0.95, 1.05)
        return $ scale * n

instance (Perturb a, Perturb b) => Perturb (a, b) where
    perturb (a, b) = do
        a' <- perturb a
        b' <- perturb b
        return (a', b')

instance Perturb a => Perturb [a] where
    perturb [] = return []
    perturb (x:xs) = do
        x' <- perturb x
        xs' <- perturb xs
        return $ x':xs'

make_problems :: IO ()
make_problems = do
    feasible <- generate $ take 10 <$> filter (not . null . solve) <$> infiniteListOf (resize 1000000 $ arbitrary :: Gen StandardForm)
    problems <- generate $ take 10 <$> filter (not . null . solve) <$> infiniteListOf (resize 5 $ elements feasible >>= perturb)
    putStr "module Randoms (problems) where\nimport Data.Clp.StandardForm\nproblems = "
    print problems

main :: IO ()
main = make_problems
--main = print $ map solve problems
