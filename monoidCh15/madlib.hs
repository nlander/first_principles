import Data.Monoid

type Verb = String
type Adjective = String
type Adverb = String
type Noun = String
type Exclamation = String

madlibbin :: Exclamation
          -> Adverb
          -> Noun
          -> Adjective
          -> String
madlibbin e adv noun adj =
  mconcat [e
         , "! he said "
         , adv
         , " as he jumped into his "
         , noun
         , " and drove off with his "
         , adj
         , " wife."]
