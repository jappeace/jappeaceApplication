-- Haskell getters and setters
module Jappie where

import           Control.Lens
import           Data.Text

-- All data is tupple (we'll ignore coproducts/sumtypes)
data Person' = Person' Text Int

getName :: Person' -> Text
getName (Person' i _) = i
getAge :: Person' -> Int
getAge (Person' _ i) = i

setName :: Person' -> Text -> Person'
setName (Person' _ b) a = Person' a b
setAge :: Person' -> Int -> Person'
setAge (Person' b _) a = Person' b a

-- Enter records (syntax support)
data Person = Person
    { person_name :: Text -- syntax support 1
    , person_age  :: Int
    }

getName' :: Person -> Text
getName' = person_name

setName' :: Person -> Int -> Person
setName' d i = d{person_age = i} -- syntax support 2

-- Lenses \o/
nameLens :: Lens' Person Text
nameLens = lens
    person_name -- getter
    (\a b -> a{person_name=b}) -- setter

-- view = use lens as getter, same as (^.)
getName'' :: Person -> Text
getName'' = view
                nameLens

-- set = use lens as setter, same as (^.)
setName'' :: Text -> Person -> Person
setName'' = set nameLens

-- Composing
data Company = Company {
   employees :: [Person]
}

employeeLens :: Lens' Company [Person]
employeeLens = lens employees (\a b -> a{employees=b})

-- now we can do stuff like:
setAllNames :: Text -> Company -> Company
setAllNames = set (employees . traverse . nameLens)

getAllNames :: Company -> [Text]
getAllNames = view (employees . traverse . nameLens)

-- the more nested structures the more usefull it becomes
-- large apps want lenses. It cuts down massively on boilerplate
-- The disadvantage complicated compile errors because of polymorpishm.
-- But once you 'learn' recognizing the errors, it becomes easier
-- For sum types you can use prisms.
