module BigInt
    exposing
        ( test
        , testResult
        , Integer
        , fromInt
        , integerDecoder
        , fromString
        , toString
        , add
        , sub
        , opposite
        , mul
        , compare
        , gt
        , gte
        , lt
        , lte
        , eq
        , neq
        , max
        , min
        )

{-| Infinite digits integers
@docs Integer
@docs fromInt
@docs fromString
@docs toString
@docs add
@docs sub
@docs opposite
@docs mul
@docs compare
@docs gt
@docs gte
@docs lt
@docs lte
@docs eq
@docs neq
@docs max
@docs min
@docs test
@docs testResult
-}

import String
import Maybe exposing (Maybe)
import Result exposing (Result)
import Char
import Basics
import Json.Decode as Decode


{- The sign of the integer -}


type Sign
    = Positive
    | Negative


type alias Digit =
    Int



{- From smallest to largest digit, all the digits are positive, no leading zeros -}


type Magnitude
    = Magnitude (List Digit)


type MagnitudeNotNormalised
    = MagnitudeNotNormalised (List Digit)


{-| Integer type
-}
type Integer
    = Integer ( Sign, Magnitude )


type IntegerNotNormalised
    = IntegerNotNormalised ( Sign, MagnitudeNotNormalised )



{- Enough to hold digit * digit without overflowing to double -}


max_digit_value =
    1000000


{-| Makes an Integer from an Int
-}
fromInt : Int -> Integer
fromInt x =
    let
        sign =
            if x < 0 then
                Negative
            else
                Positive
    in
        normalise <| IntegerNotNormalised ( sign, MagnitudeNotNormalised [ abs x ] )


{-| Makes an Integer from a String
-}
integerDecoder : Decode.Decoder Integer
integerDecoder =
    let
        convert : String -> Decode.Decoder Integer
        convert raw =
            case fromString raw of
                Just value ->
                    Decode.succeed value

                Nothing ->
                    Decode.fail <| "Failed to parse BigInt: " ++ raw
    in
        Decode.string |> Decode.andThen convert


fromString : String -> Maybe Integer
fromString x =
    case String.toList x of
        [] ->
            Just (fromInt 0)

        '-' :: xs ->
            case fromString_ xs of
                Nothing ->
                    Nothing

                Just a ->
                    Just (Integer ( Negative, a ))

        '+' :: xs ->
            case fromString_ xs of
                Nothing ->
                    Nothing

                Just a ->
                    Just (Integer ( Positive, a ))

        xs ->
            case fromString_ xs of
                Nothing ->
                    Nothing

                Just a ->
                    Just (Integer ( Positive, a ))


fromString_ : List Char -> Maybe Magnitude
fromString_ x =
    if not <| List.all Char.isDigit x then
        Nothing
    else
        let
            rev_digits =
                List.reverse x

            rev_group_digits =
                groups 6 rev_digits

            group_digits =
                List.map List.reverse rev_group_digits

            group_strings =
                List.map String.fromList group_digits

            group_result_ints =
                List.map String.toInt group_strings
        in
            let
                result_to_maybe x =
                    case x of
                        Ok a ->
                            Just a

                        Err _ ->
                            Nothing
            in
                let
                    group_maybe_ints =
                        List.map result_to_maybe group_result_ints
                in
                    let
                        gen_res x =
                            case x of
                                [] ->
                                    Just []

                                Nothing :: xs ->
                                    Nothing

                                (Just b) :: bx ->
                                    case gen_res bx of
                                        Nothing ->
                                            Nothing

                                        Just xxs ->
                                            Just (b :: xxs)
                    in
                        case gen_res group_maybe_ints of
                            Just x ->
                                Just (Magnitude x)

                            Nothing ->
                                Nothing


groups : Int -> List a -> List (List a)
groups n x =
    let
        head =
            List.take n x

        tail =
            List.drop n x
    in
        if tail == [] then
            [ head ]
        else
            head :: groups n tail


type MagnitudePair
    = MagnitudePair (List ( Digit, Digit ))


sameSize : Magnitude -> Magnitude -> MagnitudePair
sameSize (Magnitude a) (Magnitude b) =
    let
        sameSize_ a b =
            case ( a, b ) of
                ( [], [] ) ->
                    []

                ( a :: xa, b :: xb ) ->
                    ( a, b ) :: sameSize_ xa xb

                ( a :: xa, [] ) ->
                    ( a, 0 ) :: sameSize_ xa []

                ( [], b :: xb ) ->
                    ( 0, b ) :: sameSize_ [] xb
    in
        MagnitudePair (sameSize_ a b)


sameSize_ : MagnitudeNotNormalised -> MagnitudeNotNormalised -> MagnitudePair
sameSize_ (MagnitudeNotNormalised a) (MagnitudeNotNormalised b) =
    let
        sameSize_ a b =
            case ( a, b ) of
                ( [], [] ) ->
                    []

                ( a :: xa, b :: xb ) ->
                    ( a, b ) :: sameSize_ xa xb

                ( a :: xa, [] ) ->
                    ( a, 0 ) :: sameSize_ xa []

                ( [], b :: xb ) ->
                    ( 0, b ) :: sameSize_ [] xb
    in
        MagnitudePair (sameSize_ a b)


normalise : IntegerNotNormalised -> Integer
normalise (IntegerNotNormalised ( sx, x )) =
    let
        nmagnitude =
            normaliseMagnitude x
    in
        let
            is_negative_magnitude (Magnitude x) =
                case x of
                    [] ->
                        False

                    [ d ] ->
                        d < 0

                    x :: xs ->
                        is_negative_magnitude (Magnitude xs)
        in
            let
                reverse_magnitude (Magnitude xs) =
                    MagnitudeNotNormalised (List.map (\x -> 0 - x) xs)
            in
                let
                    reverse_sign s =
                        case s of
                            Positive ->
                                Negative

                            Negative ->
                                Positive
                in
                    if is_negative_magnitude nmagnitude then
                        normalise (IntegerNotNormalised ( reverse_sign sx, reverse_magnitude nmagnitude ))
                    else
                        Integer ( sx, nmagnitude )


normaliseDigit : Int -> ( Int, Digit )
normaliseDigit d =
    if d < 0 then
        let
            ( carry, d_ ) =
                normaliseDigit (d + max_digit_value)
        in
            ( carry - 1, d_ )
    else
        ( d // max_digit_value, rem d max_digit_value )


normaliseDigitList : List Int -> List Digit
normaliseDigitList x =
    case x of
        [] ->
            []

        d :: [] ->
            let
                ( c, d_ ) =
                    normaliseDigit d
            in
                if c /= 0 then
                    [ d_, c ]
                else
                    [ d_ ]

        d :: d2 :: xs ->
            let
                ( c, d_ ) =
                    normaliseDigit d
            in
                d_ :: normaliseDigitList (d2 + c :: xs)


dropWhile : (a -> Bool) -> List a -> List a
dropWhile f x =
    case x of
        [] ->
            []

        a :: xs ->
            if f a then
                dropWhile f xs
            else
                a :: xs


dropZeroes : List Digit -> List Digit
dropZeroes x =
    let
        rev_list =
            List.reverse x

        no_zeros =
            dropWhile (\x -> x == 0) rev_list
    in
        List.reverse no_zeros


normaliseMagnitude : MagnitudeNotNormalised -> Magnitude
normaliseMagnitude (MagnitudeNotNormalised x) =
    Magnitude (dropZeroes (normaliseDigitList x))


toPositiveSign : Integer -> IntegerNotNormalised
toPositiveSign (Integer ( s, Magnitude m )) =
    let
        reverse_magnitude (Magnitude xs) =
            MagnitudeNotNormalised (List.map (\x -> -x) xs)
    in
        case s of
            Positive ->
                IntegerNotNormalised ( s, MagnitudeNotNormalised m )

            Negative ->
                IntegerNotNormalised ( Positive, reverse_magnitude (Magnitude m) )


{-| Adds two Integers
-}
add : Integer -> Integer -> Integer
add a b =
    let
        (IntegerNotNormalised ( _, ma )) =
            toPositiveSign a

        (IntegerNotNormalised ( _, mb )) =
            toPositiveSign b

        (MagnitudePair p) =
            sameSize_ ma mb

        added =
            List.map (\( x, y ) -> x + y) p
    in
        normalise (IntegerNotNormalised ( Positive, MagnitudeNotNormalised added ))


{-| Changes the sign of an Integer
-}
opposite : Integer -> Integer
opposite (Integer ( s, m )) =
    let
        newsign =
            case s of
                Positive ->
                    Negative

                Negative ->
                    Positive
    in
        normalise (toPositiveSign (Integer ( newsign, m )))


{-| Substracts the second Integer from the first
-}
sub : Integer -> Integer -> Integer
sub a b =
    add a (opposite b)


{-| Multiplies two Integers
-}
mul : Integer -> Integer -> Integer
mul (Integer ( s1, m1 )) (Integer ( s2, m2 )) =
    let
        sign =
            case ( s1, s2 ) of
                ( Positive, Positive ) ->
                    Positive

                ( Negative, Negative ) ->
                    Positive

                _ ->
                    Negative
    in
        Integer ( sign, (mul_magnitudes m1 m2) )


mul_magnitudes : Magnitude -> Magnitude -> Magnitude
mul_magnitudes (Magnitude m1) (Magnitude m2) =
    case m1 of
        [] ->
            Magnitude []

        [ m ] ->
            mul_single_digit (Magnitude m2) m

        m :: mx ->
            let
                accum =
                    mul_single_digit (Magnitude m2) m

                (Magnitude rest) =
                    mul_magnitudes (Magnitude mx) (Magnitude m2)

                i1 =
                    (Integer ( Positive, accum ))

                i2 =
                    (Integer ( Positive, (Magnitude (0 :: rest)) ))

                (Integer ( _, result )) =
                    add i1 i2
            in
                result


mul_single_digit : Magnitude -> Digit -> Magnitude
mul_single_digit (Magnitude m) d =
    normaliseMagnitude (MagnitudeNotNormalised (List.map (\x -> d * x) m))


{-| Compares two Integers
-}
compare : Integer -> Integer -> Order
compare (Integer ( sa, a )) (Integer ( sb, b )) =
    let
        invert_order x =
            case x of
                LT ->
                    GT

                EQ ->
                    EQ

                GT ->
                    LT
    in
        case ( sa, sb ) of
            ( Positive, Negative ) ->
                GT

            ( Negative, Positive ) ->
                LT

            _ ->
                let
                    ss =
                        sameSize a b

                    rss =
                        reverseMagnitudePair ss

                    cr =
                        compareMagnitude rss
                in
                    if sa == Positive then
                        cr
                    else
                        invert_order cr


{-| Equals
-}
eq : Integer -> Integer -> Bool
eq a b =
    case compare a b of
        EQ ->
            True

        _ ->
            False


{-| Not equals
-}
neq : Integer -> Integer -> Bool
neq a b =
    not (eq a b)


{-| Less than
-}
lt : Integer -> Integer -> Bool
lt a b =
    case compare a b of
        LT ->
            True

        _ ->
            False


{-| Greater than
-}
gt : Integer -> Integer -> Bool
gt a b =
    case compare a b of
        GT ->
            True

        _ ->
            False


{-| Greater than or equals
-}
gte : Integer -> Integer -> Bool
gte a b =
    case compare a b of
        GT ->
            True

        EQ ->
            True

        _ ->
            False


{-| Less than or equals
-}
lte : Integer -> Integer -> Bool
lte a b =
    case compare a b of
        LT ->
            True

        EQ ->
            True

        _ ->
            False


{-| Returns the largest of two Integers
-}
max : Integer -> Integer -> Integer
max a b =
    case compare a b of
        GT ->
            a

        EQ ->
            a

        LT ->
            b


{-| Returns the smallest of two Integers
-}
min : Integer -> Integer -> Integer
min a b =
    case compare a b of
        LT ->
            a

        EQ ->
            a

        GT ->
            b


type MagnitudePairReverseOrder
    = MagnitudePairReverseOrder (List ( Digit, Digit ))


reverseMagnitudePair : MagnitudePair -> MagnitudePairReverseOrder
reverseMagnitudePair (MagnitudePair x) =
    MagnitudePairReverseOrder <| List.reverse x


compareMagnitude : MagnitudePairReverseOrder -> Order
compareMagnitude (MagnitudePairReverseOrder m) =
    case m of
        [] ->
            EQ

        ( a, b ) :: xs ->
            if a == b then
                compareMagnitude (MagnitudePairReverseOrder xs)
            else
                Basics.compare a b


zeroes : Int -> String
zeroes n =
    String.repeat n "0"


fillZeroes : Digit -> String
fillZeroes d =
    let
        d_s =
            Basics.toString d
    in
        let
            len =
                String.length d_s
        in
            zeroes (6 - len) ++ d_s


revmagnitudeToString : List Digit -> String
revmagnitudeToString m =
    case m of
        [] ->
            "0"

        [ x ] ->
            Basics.toString x

        x :: xs ->
            (Basics.toString x) ++ String.concat (List.map fillZeroes xs)


{-| Converts the Integer to a String
-}
toString : Integer -> String
toString (Integer ( s, Magnitude m )) =
    let
        sign =
            if s == Positive then
                ""
            else
                "-"
    in
        sign ++ revmagnitudeToString (List.reverse m)


{-| True if all the tests pass
-}
testResult : () -> Bool
testResult () =
    let
        t =
            test ()

        all x =
            case x of
                [] ->
                    True

                ( _, False ) :: xs ->
                    False

                ( _, True ) :: xs ->
                    all xs
    in
        all t


{-| Testsuite list of (description, True iif the test has passed)
-}
test : () -> List ( String, Bool )
test () =
    [ ( "digit size"
      , max_digit_value * max_digit_value /= (max_digit_value * max_digit_value) + 1
      )
    , ( "fromInt 1 " ++ toString (fromInt 1)
      , fromInt 1 == Integer ( Positive, Magnitude [ 1 ] )
      )
    , ( "fromInt 2 " ++ toString (fromInt -1)
      , fromInt -1 == Integer ( Negative, Magnitude [ 1 ] )
      )
    , ( "dropWhile 1"
      , dropWhile (\x -> x == 0) [ 1, 2, 3 ] == [ 1, 2, 3 ]
      )
    , ( "dropWhile 2"
      , dropWhile (\x -> x == 0) [ 0, 2, 3 ] == [ 2, 3 ]
      )
    , ( "dropWhile 3"
      , dropWhile (\x -> x == 0) [ 1, 0, 3 ] == [ 1, 0, 3 ]
      )
    , ( "dropWhile 4"
      , dropWhile (\x -> x /= 0) [] == []
      )
    , ( "toPositiveSign 1 "
            ++ toString (fromInt -1)
      , toPositiveSign (fromInt -1) == IntegerNotNormalised ( Positive, MagnitudeNotNormalised [ -1 ] )
      )
    , ( "toPositiveSign 2 "
            ++ toString (fromInt 1)
      , toPositiveSign (fromInt 1) == IntegerNotNormalised ( Positive, MagnitudeNotNormalised [ 1 ] )
      )
    , ( "sameSize 1"
      , sameSize (Magnitude [ 1, 2, 3 ]) (Magnitude [ 1, 2 ]) == MagnitudePair ([ ( 1, 1 ), ( 2, 2 ), ( 3, 0 ) ])
      )
    , ( "normaliseMagnitude 1"
      , normaliseMagnitude (MagnitudeNotNormalised [ 1, 2, 3 ]) == Magnitude [ 1, 2, 3 ]
      )
    , ( "normaliseMagnitude 2"
      , normaliseMagnitude (MagnitudeNotNormalised [ 1, 0, 0 ]) == Magnitude [ 1 ]
      )
    , ( "normaliseMagnitude 3"
      , normaliseMagnitude (MagnitudeNotNormalised [ -1 ]) == Magnitude [ max_digit_value - 1, -1 ]
      )
    , ( "normaliseMagnitude 4"
      , normaliseMagnitude (MagnitudeNotNormalised [ -1, 1 ]) == Magnitude [ max_digit_value - 1 ]
      )
    , ( "add 1 "
      , add (fromInt 1) (fromInt 2) == (fromInt 3)
      )
    , ( "opposite 1 "
            ++ toString (opposite (fromInt -1))
      , opposite (fromInt -1) == fromInt 1
      )
    , ( "opposite 2 "
            ++ toString (opposite (fromInt 1))
      , opposite (fromInt 1) == fromInt -1
      )
    , ( "sub 1 "
            ++ toString (sub (fromInt 1) (fromInt 2))
      , sub (fromInt 1) (fromInt 2) == (fromInt -1)
      )
    , ( "mul 1 "
      , mul (fromInt 3) (fromInt 2) == (fromInt 6)
      )
    , ( "mul 2 "
      , mul (fromInt (3 * max_digit_value)) (fromInt (2 * max_digit_value)) == Integer ( Positive, Magnitude [ 0, 0, 6 ] )
      )
    , ( "mul 3 "
      , mul (Integer ( Positive, Magnitude [ 2, 2 ] )) (Integer ( Negative, Magnitude [ 2, 2 ] )) == Integer ( Negative, Magnitude [ 4, 8, 4 ] )
      )
    , ( "mul 4"
      , mul (fromInt 1234567890) (fromInt 1234567890) == Integer ( Positive, Magnitude (List.reverse [ 1, 524157, 875019, 52100 ]) )
      )
    , ( "mul 5"
      , mul (fromInt 111111111) (fromInt 111111111) == Integer ( Positive, Magnitude (List.reverse [ 12345, 678987, 654321 ]) )
      )
    , ( "toString 1 " ++ (toString <| Integer ( Positive, Magnitude (List.reverse [ 1, 524157, 875019, 52100 ]) ))
      , "1524157875019052100" == (toString <| Integer ( Positive, Magnitude (List.reverse [ 1, 524157, 875019, 52100 ]) ))
      )
    , ( "toString 2 " ++ (toString <| Integer ( Negative, Magnitude (List.reverse [ 1, 524157, 875019, 52100 ]) ))
      , "-1524157875019052100" == (toString <| Integer ( Negative, Magnitude (List.reverse [ 1, 524157, 875019, 52100 ]) ))
      )
    , ( "toString 3 " ++ (toString <| Integer ( Positive, Magnitude [] ))
      , "-0" == (toString <| Integer ( Negative, Magnitude [] ))
      )
    , ( "toString 4 " ++ (toString <| Integer ( Positive, Magnitude [ 0 ] ))
      , "0" == (toString <| Integer ( Positive, Magnitude [ 0 ] ))
      )
    , ( "fromString 1"
      , Just (fromInt 1) == fromString "1"
      )
    , ( "fromString 2"
      , Just (fromInt -1) == fromString "-1"
      )
    , ( "fromString 3"
      , fromString "-a" == Nothing
      )
    , ( "fromString 4"
      , fromString "1234567890" == Just (Integer ( Positive, Magnitude [ 567890, 1234 ] ))
      )
    , ( "max 1"
      , max (fromInt 1234567890) (fromInt 3) == fromInt 1234567890
      )
    , ( "max 2"
      , max (fromInt 1) (fromInt 3) == fromInt 3
      )
    , ( "min 1"
      , min (fromInt 1234567890) (fromInt 3) == fromInt 3
      )
    , ( "min 2"
      , min (fromInt 1) (fromInt 3) == fromInt 1
      )
    , ( "compare 1"
      , compare (fromInt 1234567890) (fromInt 3) == GT
      )
    , ( "compare 2"
      , compare (fromInt 3) (fromInt 3) == EQ
      )
    , ( "compare 3"
      , compare (fromInt 1) (fromInt 3) == LT
      )
    , ( "gt 1"
      , gt (fromInt 1234567890) (fromInt 3) == True
      )
    , ( "gt 2"
      , gt (fromInt 3) (fromInt 3) == False
      )
    , ( "gt 3"
      , gt (fromInt 1) (fromInt 3) == False
      )
    , ( "gte 1"
      , gte (fromInt 1234567890) (fromInt 3) == True
      )
    , ( "gte 2"
      , gte (fromInt 3) (fromInt 3) == True
      )
    , ( "gte 3"
      , gte (fromInt 1) (fromInt 3) == False
      )
    , ( "eq 1"
      , eq (fromInt 1234567890) (fromInt 3) == False
      )
    , ( "eq 2"
      , eq (fromInt 3) (fromInt 3) == True
      )
    , ( "lt 1"
      , lt (fromInt 1234567890) (fromInt 3) == False
      )
    , ( "lt 2"
      , lt (fromInt 3) (fromInt 3) == False
      )
    , ( "lt 3"
      , lt (fromInt 1) (fromInt 3) == True
      )
    , ( "lte 1"
      , lte (fromInt 1234567890) (fromInt 3) == False
      )
    , ( "lte 2"
      , lte (fromInt 3) (fromInt 3) == True
      )
    , ( "lte 3"
      , lte (fromInt 1) (fromInt 3) == True
      )
    ]
