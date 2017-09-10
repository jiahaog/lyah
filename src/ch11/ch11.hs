import Control.Applicative (liftA2)

data CMaybe a
  = CNothing
  | CJust Int
          a
  deriving (Show)

instance Functor CMaybe where
  fmap f CNothing = CNothing
  fmap f (CJust counter x) = CJust (counter + 1) (f x)

sequenceA' :: (Applicative f) => [f a] -> f [a]
sequenceA' [] = pure []
sequenceA' (x:xs) = (:) <$> x <*> sequenceA' xs

sequenceA'' :: (Applicative f) => [f a] -> f [a]
sequenceA'' = foldr (liftA2 (:)) (pure [])

{- Deliberately modified type constructor -}
data ZipList a = AZipList
  { getZipList :: [a]
  } deriving (Show)

newtype CharList = CharList
  { getCharList :: [Char]
  } deriving (Eq, Show)

newtype Pair b a = Pair
  { getPair :: (a, b)
  }

instance Functor (Pair c) where
  fmap f (Pair (x, y)) = Pair (f x, y)

newtype CoolBool = CoolBool
  { getCoolBool :: Bool
  }

helloMe :: CoolBool -> String
helloMe (CoolBool _) = "Hello"

data Tree a
  = EmptyTree
  | Node a
         (Tree a)
         (Tree a)
  deriving (Show, Read, Eq)

singleton :: a -> Tree a
singleton x = Node x EmptyTree EmptyTree

treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x EmptyTree = singleton x
treeInsert x (Node a left right)
  | x == a = Node x left right
  | x < a = Node a (treeInsert x left) right
  | x > a = Node a left (treeInsert x right)

treeElem :: Ord a => a -> Tree a -> Bool
treeElem x EmptyTree = False
treeElem x (Node a left right)
  | x == a = True
  | x < a = treeElem x left
  | x > a = treeElem x right

instance Foldable Tree where
  foldMap f EmptyTree = mempty
  foldMap f (Node x l r) = foldMap f l `mappend` f x `mappend` foldMap f r

testTree =
  Node
    10
    (Node 5 (Node 1 EmptyTree EmptyTree) (Node 6 EmptyTree EmptyTree))
    (Node 15 (Node 14 EmptyTree EmptyTree) (Node 17 EmptyTree EmptyTree))
