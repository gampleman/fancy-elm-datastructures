module Ring exposing (Ring, forward, fromList, get, insertAfter, length, map, map2, set, singleton, take, toList)

import Array exposing (Array)


type Ring a
    = Ring Int a (Array ( a, Int, Int ))


singleton : a -> Ring a
singleton el =
    Ring 0 el (Array.fromList [ ( el, 0, 0 ) ])


insertAfter : a -> Ring a -> Ring a
insertAfter el ((Ring currIndex default arr) as ring) =
    let
        ( currEl, prevIndex, nextIndex ) =
            internalGet ring

        newArr =
            arr
                |> Array.set currIndex ( currEl, prevIndex, Array.length arr )
                |> Array.push ( el, currIndex, nextIndex )
    in
    Ring currIndex default newArr


fromList : ( a, List a ) -> Ring a
fromList ( head, tail ) =
    let
        last =
            List.length tail
    in
    (head :: tail)
        |> List.indexedMap
            (\i el ->
                ( el
                , if i == 0 then
                    last

                  else
                    i - 1
                , if i == last then
                    0

                  else
                    i + 1
                )
            )
        |> Array.fromList
        |> Ring 0 head


set : a -> Ring a -> Ring a
set el ((Ring currIndex default arr) as ring) =
    let
        ( _, prevIndex, nextIndex ) =
            internalGet ring
    in
    Ring currIndex el (Array.set currIndex ( el, prevIndex, nextIndex ) arr)


get : Ring a -> a
get (Ring _ current _) =
    current


forward : Ring a -> Ring a
forward ((Ring currIndex default arr) as ring) =
    let
        ( _, prevIndex, nextIndex ) =
            internalGet ring

        ( el, _, _ ) =
            internalGet (Ring nextIndex default arr)
    in
    Ring nextIndex el arr


{-|

    Ring.fromList (A, [B])
        |> Ring.forward
        |> Ring.take 5
        --> [ B, A, B, A, B ]

-}
take : Int -> Ring a -> List a
take howMany (Ring currIndex default arr) =
    let
        takeHelp n index soFar =
            if n > 0 then
                let
                    ( el, _, nextIndex ) =
                        Array.get index arr
                            -- this should never happen
                            |> Maybe.withDefault ( default, currIndex, currIndex )
                in
                takeHelp (n - 1) nextIndex (el :: soFar)

            else
                List.reverse soFar
    in
    takeHelp howMany currIndex []


toList : Ring a -> List a
toList ring =
    take (length ring) ring


length : Ring a -> Int
length (Ring _ _ arr) =
    Array.length arr


map : (a -> b) -> Ring a -> Ring b
map fn (Ring currIndex default arr) =
    Ring currIndex (fn default) (Array.map (\( el, prev, next ) -> ( fn el, prev, next )) arr)


{-| map2 (and friends) has some unusual behaviors when combining Rings:

1.  It will align the two rings so that the focus elements match.
2.  Lists are `List.length (List.map2 Tuple.pair a b) == min (List.length a) (List.length b)`, but rings are `Ring.length (Ring.map2 Tuple.pair a b) == max (Ring.length a) (Ring.length b)`. In other words the smaller ring will be repeated to fill out the length of the larger ring.
3.  Achieving the above two properties burns some performance. Specifically we need to use the linked list style traversal which is slightly slower than directly mapping Arrays. Also List.map2 is O(min(a,b)), this is O(max(a,b)).

If you don't care for the above properties, you should probably use toList, and fromList.

    Ring.map2 Tuple.pair (Ring.fromList (A, [B])) (Ring.fromList (1, [2, 3]))
      --> Ring.fromList ( (A, 1), [ (B, 2), (A, 3)])

-}
map2 : (a -> b -> c) -> Ring a -> Ring b -> Ring c
map2 fn (Ring aCurrIndex aDefault aArr) (Ring bCurrIndex bDefault bArr) =
    let
        steps =
            max (Array.length aArr) (Array.length bArr)

        stepper n aIndex bIndex arr =
            if n < steps then
                let
                    ( a, _, aNextIndex ) =
                        Array.get aIndex aArr
                            -- this should never happen
                            |> Maybe.withDefault ( aDefault, aCurrIndex, aCurrIndex )

                    ( b, _, bNextIndex ) =
                        Array.get bIndex bArr
                            -- this should never happen
                            |> Maybe.withDefault ( bDefault, bCurrIndex, bCurrIndex )
                in
                Array.push
                    ( fn a b
                    , if n == 0 then
                        steps - 1

                      else
                        n - 1
                    , if n == steps - 1 then
                        0

                      else
                        n + 1
                    )
                    arr
                    |> stepper (n + 1) aNextIndex bNextIndex

            else
                arr
    in
    Ring 0 (fn aDefault bDefault) (stepper 0 aCurrIndex bCurrIndex Array.empty)



-- internal


internalGet : Ring a -> ( a, Int, Int )
internalGet (Ring currIndex default arr) =
    Array.get currIndex arr
        -- this should never happen
        |> Maybe.withDefault ( default, currIndex, currIndex )
