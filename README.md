# push-pull-inst-proto

Definitions of and instances for `Push` and `Pull` along with experiments to generalize them.


## Push and Pull

Definitions of `Push` and `Pull` as Haskell type-classes:

```haskell
class (Functor f, Functor g) => Push f g where
  part :: f (g a)     -> f (f (g a))
  pver :: f (f (g a)) -> f (g (f a))
  push :: f (g a)     -> f (g (f a))
  push = pver . part

class Pull f g where
  draw :: f (g (f a)) -> f (f (g a))
  bond :: f (f (g a)) -> f (g a)
  pull :: f (g (f a)) -> f (g a)
  pull = bond . draw
```


### Instances

The following instances for `Push`, `Pull` are provided:

```haskell
instance Functor g => Push IO g
instance Push NonEmpty Maybe
instance Push Tree Maybe

instance Push [] Maybe
instance Alternative g => Push [] (Cofree g)
instance Applicative g => Push [] (Free g)
```


## Generalizing

### Category theory

An implementation of categories as a GADT, where
the index is typed using a phantom type variable:

```haskell
infixr 1 :~
newtype (:~) a b = Ty { getTy :: a }

data Cat a where
  Obj :: a -> Cat (a :~ Cat a)
  Mor :: (a -> b) -> Cat ((a -> b) :~ (Cat a -> Cat b))
  Comp :: Cat ((b -> c) :~ (Cat b -> Cat c)) -> Cat ((a -> b) :~ (Cat a -> Cat b)) -> Cat ((a -> c) :~ (Cat a -> Cat c))
  Id :: Cat ((a -> a) :~ (Cat a -> Cat a))
```


### To source transformations

Several recursive functions in different organizations of their recursive calls are provided,
in an attempt to model the source code transformations provided by `Push`, `Pull`,
and compare their respective performance.


# Docs

Haddock-generated documentation is available [here](https://michaeljklein.github.io/push-pull-inst-proto/)

