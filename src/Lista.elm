module Lista exposing (..)

import Native.Lista
import Array

shuffle : List a -> List a
shuffle lista1 =
  let
    lista2 = Array.fromList lista1
    lista3 = Native.Lista.shuffle lista2
    lista4 = Array.toList lista3
  in
    lista4
  

getElements : Int -> List a -> (List a, List a)
getElements num lista1 =
  let
    lista2 = Array.fromList lista1
    (droped, items) = Native.Lista.getElements num lista2
    items2 = Array.toList items
    droped2 = Array.toList droped
  in
    (droped2, items2)
    