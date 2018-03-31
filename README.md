# Tabler

This is an attempt at a data storage application in which the schema is
user-defined; the hard part is figuring out how to use type-level programming
techniques in Haskell to improve correctness of the data storage functions.

## What is this about?

In most webapps, domain models are usually hardedcoded. For example,
an e-commerce webapp might have a store item encoded as something similar to

```haskell
data StoreItem =
  MkStoreItem {
    getId :: Int,
    getName :: String,
    getDesc :: String,
    getPrice :: Double
  }
```

and a shopping cart may be encoded as

```haskell
data CartEntry =
  MkCartEntry {
    getId :: Int,
    getQuatity :: Int
  }

newtype Cart = MkCart [CartEntry]
```

Generally, most of these data-driven webapps represent their domain models as
record types. Businesses usually store such data in databases, (hopefully)
making sure to ensure that only valid data is stored. Individuals managing data
for personal consumption (e.g. grocery lists, todo-lists, etc.) may prefer to
use spreadsheets instead. While spreadsheets are far simpler than databases,
they do tend to lose the data validation aspects, which could be problematic for
data entry. Additionally, databases have the upper hand in terms of searching
and filtering capabilities.

It would be nice to have a middle ground: have a custom, user-defined schema and
allows for validated data entry and enhanced search and filtering.

## Implementation challenges

To implement this proposed webapp, we first need some sort of data type to
_describe_ a schema. For simplicity, we'll only allow schemas to be strings,
ints, and products of strings and ints.

```haskell
data Schema = SchString
            | SchInt
            | (:+:) Schema Schema

infixr 5 :+:

-- A pair of an int and a string
exampleSchema = SchString :+: SchInt

-- A cart entry is just a pair of ints
cartEntrySchema = SchInt :+: SchInt
```

The challenge is in encoding a _representation_ of such a schema. An initial
attempt may be something like

```haskell
-- Here, we just use a pair representation, but it can also be a record
data SchemaRepr = RString String
                | RInt Int
                | RPair SchemaRepr SchemaRepr

-- How a cart entry might be stored
cartEntryRepr :: SchemaRepr
cartEntryRepr = RPair (RInt 5) (RInt 10)
```

but this makes it quite tedious to write operations on a schema representation.
Suppose we want to filter a list of records by some key.

```haskell
filter :: Schema -> SchemaRepr -> [SchemaRepr] -> [SchemaRepr]
```

However, to write such a function, we need to make the following assumptions:

* the key is needs to be a value in the schema.
* every record in the list needs to contain the given key
* the type of the key in the record needs to match the type of the input key

These are "obvious" facts, since (hopefully) the data had been validated during
insertion. However, these facts are not reflected in the type -- all we have is
information about "some" representation. Thus, we have no choice but to perform
validation every single time `filter` or any other function that operates on the
data is called, even if the data is not modified.

The representation needs to be changed to reflect the fact that the
representation is _completely_ determined by the schema. It does not make sense
to say "I have a representation"; rather, we must say "I have a representation
of this particular schema". Thus, given some schema _value_, there should be
a corresponding representation _type_. This yields a function from values to types

```haskell
type family SchemaRepr (s :: Schema) :: * where
  SchemaRepr SchString = String
  SchemaRepr SchInt = Int
  SchemaRepr (a :+: b) = (SchemaRepr a, SchemaRepr b)
```

which, in theory, could let us write `filter` like

```haskell
filter :: (s :: Schema) -> SchemaRepr s -> [SchemaRepr s] -> [SchemaRepr s]
```

For example, if our schema is `SchString :+: SchInt`, the type of `filter`
becomes

```haskell
filter :: (SchString :+: SchInt) -> (Int, String) -> [(Int, String)] -> [(Int, String)]
```

However, in practice, implementing such a function is either very difficult or
impossible in Haskell, because Haskell only partially supports dependent types
(in its current state, at least; see "Dependent Haskell"). The purpose of the
code in this repository is to see how far we can go. See
[Schema.hs](lib/Schema.hs) for concrete attempts at the problem.
