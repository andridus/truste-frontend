module Uuid exposing (gen, random)

import Native.Uuid

gen : Int -> String
gen id =
  Native.Uuid.gen id


random : Int -> Int
random i =
  Native.Uuid.random i