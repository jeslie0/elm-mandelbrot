module ColourMaps.Internal exposing (colourMapDataToFunction, colourMapDataToFunctionRefined, colourMapListsToArray)

import Array exposing (Array)


colourMapListsToArray : List (List a) -> Array (Array a)
colourMapListsToArray lst =
    Array.map Array.fromList <| Array.fromList lst


{-| Int is a number in the range 0 - 255. This extracts the element
from the given dataArray at that index and processes it into the
correct type.
-}
colourMapDataToFunction : Array (Array Float) -> Int -> Maybe { red : Float, green : Float, blue : Float, alpha : Float }
colourMapDataToFunction dataArray n =
    let
        data =
            Array.get n dataArray

        redM =
            Maybe.andThen (Array.get 0) data

        greenM =
            Maybe.andThen (Array.get 1) data

        blueM =
            Maybe.andThen (Array.get 2) data
    in
    Maybe.map3 (\r g b -> { red = r, green = g, blue = b, alpha = 1 }) redM greenM blueM


{-| Float is a value between 0 and 1. This function converts it into a
float between 0 and 255, then extracts the pair of values it is
closest to, averages them, then returns the result in the given type.
-}
colourMapDataToFunctionRefined : Array (Array Float) -> Float -> Maybe { red : Float, green : Float, blue : Float, alpha : Float }
colourMapDataToFunctionRefined dataArray val =
    let
        scaledVal =
            255 * val

        roundedScaledVal =
            round scaledVal

        scaledValIsInt =
            toFloat roundedScaledVal - scaledVal == 0
    in
    if scaledValIsInt then
        colourMapDataToFunction dataArray roundedScaledVal

    else
        let
            intLessThan =
                floor scaledVal

            intGreaterThan =
                ceiling scaledVal

            lowerData =
                Array.get intLessThan dataArray

            lowerRedM =
                Maybe.andThen (Array.get 0) lowerData

            lowerGreenM =
                Maybe.andThen (Array.get 1) lowerData

            lowerBlueM =
                Maybe.andThen (Array.get 2) lowerData

            upperData =
                Array.get intGreaterThan dataArray

            upperRedM =
                Maybe.andThen (Array.get 0) upperData

            upperGreenM =
                Maybe.andThen (Array.get 1) upperData

            upperBlueM =
                Maybe.andThen (Array.get 2) upperData

            calculateWeightedMidPoint newStart newEnd =
                let
                    lowerDiff =
                        scaledVal - toFloat intLessThan

                    initialRange =
                        intGreaterThan - intLessThan

                    finalRange =
                        newEnd - newStart
                in
                (lowerDiff * finalRange) / toFloat initialRange + newEnd
        in
        Maybe.map3 (\r g b -> { red = r, green = g, blue = b, alpha = 1 })
            (Maybe.map2 calculateWeightedMidPoint lowerRedM upperRedM )
            (Maybe.map2 calculateWeightedMidPoint lowerGreenM upperGreenM )
            (Maybe.map2 calculateWeightedMidPoint lowerBlueM upperBlueM )
