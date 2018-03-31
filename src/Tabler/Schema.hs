{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ExistentialQuantification #-}

module Tabler.Schema where

import Data.Singletons
import Data.Singletons.TH

-- Define our extensible schema, and let's automatically derive singletons
-- to have better support for dependent types.
$(singletons [d|
  data Schema = SchString
              | SchInt
              | (:+:) Schema Schema |])
infixr 5 :+:

instance Show Schema where
  show SchString = "String"
  show SchInt = "Int"
  show (s1 :+: s2) = "(" ++ show s1 ++ ", " ++ show s2 ++ ")"

-- | Type level function that maps a schema description to a Haskell type
-- | E.g. we want a (SchString :+: SchInt) to map to (String, Int), which
-- | gives (SchemaRepr (SchString :+: SchInt)) = (String, Int)
type family SchemaRepr (s :: Schema) :: * where
  SchemaRepr SchString = String
  SchemaRepr SchInt = Int
  SchemaRepr (a :+: b) = (SchemaRepr a, SchemaRepr b)

-- | Since the representation type is determined at runtime, we need a way to
-- | print out a representation. Show doesn't work here because Show can't be
-- | implemented for a type family. Here, a singleton helps keep track of the
-- | static type at runtime.
showRepr :: forall s. (SingI s) => SchemaRepr s -> String
showRepr repr = withSing @s $ \s -> case fromSing s of
  sch -> show sch

-- | Dummy function to "parse" a string into a schema
defineSchema :: String -> Schema
defineSchema "a" = SchString :+: SchInt
defineSchema "b" = SchInt
defineSchema _ = SchString

-- | Represents a collection of types that adhere to a given schema.
type DataStore s = [SchemaRepr s]

-- | Creates an empty DataStore
emptyStore :: Sing s -> DataStore s
emptyStore _ = []

-- We can easily create and manipulate a DataStore if the schema is known
-- at compile time.
example_sstore :: DataStore SchString
example_sstore = emptyStore SSchString ++ ["foo"]

-- What if the schema is only known at runtime (e.g. a user defines it at
-- runtime)? A first attempt might be
--
--   initStore :: Schema -> DataStore s
--
-- but another look at the type will make it clear that it won't work. The
-- type signature can be written more explicitly as
--
--   initStore :: forall s. Schema -> DataStore s
--
-- Which says that given an arbitrary schema, initStore can build a DataStore
-- that stores ANY type of schema. That is, the following could "type-check"
-- but not reflect reality:
--
--   initStore SchString :: DataStore SchInt
--
-- Also, it would be incredibly difficult, if not, impossible to implement the
-- function (try it yourself!).

-- The function can be used by

-- | Generates an empty data store for the given schema, applying the given
-- | function to the empty data store.
initStore :: Schema -> (forall s. DataStore s -> r) -> r
initStore s f = withSomeSing s $ \s' -> case s' of
  ev@SSchString -> f @SchString (emptyStore ev)
  ev@SSchInt -> f @SchInt (emptyStore ev)
  ev@((:%+:) (_ :: Sing a) (_ :: Sing b)) -> f @(a :+: b) (emptyStore ev)

-- initStore is very difficult to use because writing a useful function with the
-- type
--   (forall s. DataStore s -> r)
-- is quite hard. For example, r cannot be DataStore s, because s is bound.

-- Another example: reading a String into a SchemaRepr. Suppose we know what the
-- schema is at runtime. We want to pass the schema to SchemaRepr, but Haskell
-- separates types and values. We can pattern match on the schema to determine
-- how to read the input string.
readValue :: forall (s :: Schema) r. SingI s => String -> (forall s'. SingI s' => SchemaRepr s' -> r) -> Maybe r
readValue str f = withSing @s $ \s -> case fromSing s of
  SchString -> Just $ f @SchString str
  SchInt -> Just $ f @SchInt 5 -- placeholder
  (a :+: b) -> Nothing -- placeholder

-- Examples
mySchema = SchString :+: SchString :+: SchInt

-- For example, the following function fails to typecheck.
--
-- foo = withSomeSing mySchema $ \(s :: Sing (a :: Schema)) -> withSingI @a @(Maybe String) s $
--   readValue @a @String "asdf" f -- How to implement this?
--   where f :: forall s'. SingI s' => SchemaRepr s' -> String
--         f = showRepr @s'

-- What we really need is a dependent type known as a pi type, which is a type
-- that is determined by some input value. In other words, a function from a value
-- to a type. It could be written in Dependent Haskell as
--
--   readValue :: pi (s :: Schema) -> String -> Maybe (SchemaRepr s)

-- | Another attempt: use an existential type
data DStoreSigma = forall (s :: Schema). (SingI s) => MkDSigma (DataStore s)
data ReprSigma = forall (s :: Schema). (SingI s) => MkRSigma (SchemaRepr s)

mySigma = MkDSigma @SchString ["foo"]

initStore' :: Schema -> DStoreSigma
initStore' s = withSomeSing @Schema @DStoreSigma s $ \s' -> case s' of
  ev@SSchString -> MkDSigma @SchString (emptyStore ev)
  ev@SSchInt -> MkDSigma @SchInt (emptyStore ev)
  ev@((:%+:) (sa :: Sing a) (sb :: Sing b)) -> withSingI (sa :%+: sb) $
    MkDSigma @(a :+: b) (emptyStore ev)

readValue' :: forall (s :: Schema) r. SingI s => String -> Maybe ReprSigma
readValue' str = withSing @s $ \s -> case fromSing s of
  SchString -> Just $ MkRSigma @SchString str
  SchInt -> Just $ MkRSigma @SchInt 5 -- placeholder
  (a :+: b) -> Nothing -- placeholder
