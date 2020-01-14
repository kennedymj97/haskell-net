port module Main exposing (..)

import Array as A
import Browser
import Html exposing (Html, a, button, canvas, div, h1, h2, h3, li, p, text, ul)
import Html.Attributes as HA exposing (class, href, id)
import Html.Events exposing (onClick)
import Http
import Json.Decode
import Json.Encode
import List
import String
import Svg
import Svg.Attributes as SA exposing (x1, x2, y1, y2)



-- MAIN


main =
    Browser.document
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type Model
    = Initial
    | Failure
    | Loading
    | Probs (List Float)


init : () -> ( Model, Cmd Msg )
init _ =
    ( Initial, Cmd.none )



-- UPDATE


type Msg
    = SendImage (A.Array Float)
    | GotProbs (Result Http.Error (List Float))
    | Reset


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SendImage img ->
            -- CMD will not be none here! Need to send a request to the api!
            ( Loading, postImg img )

        GotProbs result ->
            case result of
                Ok data ->
                    ( Probs data, Cmd.none )

                Err _ ->
                    ( Failure, Cmd.none )

        Reset ->
            ( Initial, Cmd.none )


getPrediction : List Float -> String
getPrediction xs =
    extracitIdx <| List.maximum <| List.indexedMap (\x y -> ( y, x )) xs


indexProbs : List Float -> List ( Float, Int )
indexProbs =
    List.indexedMap (\x y -> ( y, x ))


extracitIdx : Maybe ( Float, Int ) -> String
extracitIdx t =
    case t of
        Just ( x, y ) ->
            String.fromInt y

        Nothing ->
            "-"


getCertainty : List Float -> String
getCertainty xs =
    case List.maximum xs of
        Just x ->
            probToPercentage x

        Nothing ->
            "-"


probToPercentage : Float -> String
probToPercentage prob =
    if prob == 0 then
        ""

    else if prob < 0.01 then
        "<1%"

    else
        String.fromFloat (prob * 100) |> String.left 4 |> (\perc -> perc ++ "%")


visualiseProbs : List Float -> List (Html msg)
visualiseProbs xs =
    case xs of
        [] ->
            List.map visualiseProb (List.repeat 10 0 |> indexProbs)

        _ ->
            List.map visualiseProb (indexProbs xs)


visualiseProb : ( Float, Int ) -> Html msg
visualiseProb ( prob, idx ) =
    div [ class "visualise-prob" ]
        [ text (String.fromInt idx ++ ": ")
        , probabilityLine prob
        , text (probToPercentage prob)
        ]


probabilityLine : Float -> Html msg
probabilityLine val =
    Svg.svg
        [ HA.style "width" "90%", HA.style "height" "10px" ]
        [ Svg.line
            [ x1 "3.5%"
            , y1 "50%"
            , x2 ((String.fromFloat <| 3.5 + (val * 93)) ++ "%")
            , y2 "50%"
            , SA.stroke "rgb(255,0,0)"
            , SA.strokeWidth "6"
            , SA.strokeLinecap "round"
            ]
            []
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    numChanged (Json.Decode.decodeValue numDecoder >> getProbs)


port numChanged : (Json.Encode.Value -> msg) -> Sub msg


numDecoder : Json.Decode.Decoder (A.Array Float)
numDecoder =
    Json.Decode.field "data" (Json.Decode.array Json.Decode.float)



-- VIEW


type alias Document msg =
    { title : String
    , body : List (Html msg)
    }


view : Model -> Document Msg
view model =
    { title = "Deep Haskell"
    , body =
        [ div [ class "page" ]
            [ div [ class "heading" ] [ h1 [] [ text "HaskellNet" ] ]
            , div [ class "content" ]
                [ div [ class "left" ]
                    [ div [ id "canvas-container" ] [ canvas [ id "canvas" ] [] ]
                    , button [ id "reset", onClick Reset ] [ text "Reset" ]
                    , div [ class "vis-container" ] (viewProbs model)
                    ]
                , div [ class "right" ] projectInfo
                ]
            ]
        ]
    }


viewProbs : Model -> List (Html Msg)
viewProbs model =
    case model of
        Initial ->
            [ text "Draw a number in the box above!" ]

        Failure ->
            [ text "Couldn't get a result back from the model :'(" ]

        Loading ->
            [ text "Getting predictions" ]

        Probs probs ->
            [ div [ class "prediction" ]
                [ div [ class "prediction-item" ] [ text <| "Prediction: " ++ getPrediction probs ]
                , div [ class "prediction-item" ] [ text <| "    Certainty: " ++ getCertainty probs ]
                ]
            , div [ class "visualise-probs" ] (visualiseProbs probs)
            ]



-- HTML


postImg : A.Array Float -> Cmd Msg
postImg img =
    Http.post
        { url = "http://localhost:8081"
        , body = Http.jsonBody (encodeImg img)
        , expect = Http.expectJson GotProbs decodeProbs
        }


encodeImg : A.Array Float -> Json.Encode.Value
encodeImg img =
    Json.Encode.object
        [ ( "img", Json.Encode.array Json.Encode.float img )
        ]


decodeProbs : Json.Decode.Decoder (List Float)
decodeProbs =
    Json.Decode.field "probs" (Json.Decode.list Json.Decode.float)


getProbs : Result Json.Decode.Error (A.Array Float) -> Msg
getProbs result =
    case result of
        Ok raw ->
            SendImage (formatRawData raw)

        Err _ ->
            Reset


formatRawData : A.Array Float -> A.Array Float
formatRawData =
    convertToGray >> downsampleImg


convertToGray : A.Array Float -> A.Array Float
convertToGray arr =
    A.fromList <|
        List.foldr
            (\( idx, val ) acc ->
                if modBy 4 idx == 0 then
                    val :: acc

                else
                    acc
            )
            []
            (A.toIndexedList arr)


downsampleImg : A.Array Float -> A.Array Float
downsampleImg arr =
    let
        l =
            A.length arr

        n =
            sqrt (toFloat l)

        z =
            n / 28

        zf =
            floor z
    in
    A.fromList <|
        List.foldr
            (\idx acc -> calcPixelVal n zf (topLeftIdx n z idx) arr :: acc)
            []
            (List.range 0 783)


calcPixelVal : Float -> Int -> Int -> A.Array Float -> Float
calcPixelVal n zf idx arr =
    255
        - (List.foldr
            (\y acc ->
                List.foldr
                    (\x acc2 -> acc2 + Maybe.withDefault 255 (A.get (y + x) arr))
                    acc
                    (List.range 0 (zf - 1))
            )
            0
            (List.map (\val -> idx + (val * round n)) (List.range 0 (zf - 1)))
            / toFloat (zf ^ 2)
          )


sumArray : A.Array Float -> Float
sumArray =
    A.foldr (+) 0


topLeftIdx : Float -> Float -> Int -> Int
topLeftIdx n z idx =
    floor (toFloat (modBy 28 idx) * z) + roundToNearestMult n (n * z * toFloat (idx // 28))


roundToNearestMult : Float -> Float -> Int
roundToNearestMult r x =
    x / r |> round |> toFloat |> (*) r |> floor


projectInfo : List (Html Msg)
projectInfo =
    [ h2 [ id "intro" ] [ text "Introduction" ]
    , p []
        [ text "This project largely inspired by a blog series on "
        , a [ href "https://blog.jle.im/entry/practical-dependent-types-in-haskell-1.html" ] [ text "type-safe neural networks by Justin Le." ]
        , text
            """
               It provides a library to produce artificial neural networks in Haskell. This site interacts with the library by sending the data of a number you draw to an API that will provide a prediction from a network trained using the library on the 
               """
        , a [ href "http://yann.lecun.com/exdb/mnist/" ] [ text "MNIST dataset. " ]
        , text "The network is by no means optimal but it achieves the goal of 95% prediction accuracy set at the start of the project."
        ]
    , p []
        [ text
            """
                  Machine learning code is notoriously hard to test and debug. There is no simple way to check for correctness and there are many places where bugs can occur from incorrect matrix multiplications. This project experiments with the use of dependant types to ensure that the matrix multiplications are valid. It does not solve the issue but has a significant impact in helping catch bugs. There were times in writing the library that I would mistakenly multiply the wrong matrices, instead of having to go through the pain of trying to work out exactly why the results were weird the compiler threw an error, letting me known exactly where and why the code would not work.
               """
        ]
    , h2 [] [ text "Key Takeaways" ]
    , ul []
        [ li [] [ text "Learning more about functional programming in general" ]
        , li [] [ text "The power of types and type-level programming" ]
        , li [] [ text "Deeper understanding of the fundamentals of neural networks" ]
        , li [] [ text "Learning about and experimenting with ", a [ href "https://elm-lang.org/" ] [ text "Elm" ] ]
        ]
    , h2 [] [ text "Improvements" ]
    , h3 [] [ text "Data Augmentation" ]
    , p []
        [ text
            """
                If you have had a chance to mess around with drawing some numbers you may have noticed some flaws. The most glaring to me is that the position and size you draw the number matters. For example, if drawing a 1 towards the right of the box it can be very confident you are drawing a 4! This is because the MNIST dataset contains numbers that are all of a similar size and are centered in the box. Now, this wouldn't be a problem if you could control the input, but in this case you can draw the number at whatever size and position you want.
            """
        , p []
            [ text
                """
                To reduce this problem data augmentation could be used. This means the original dataset would be taken and a set of transforms applied to the images. Namely moving the numbers around, resizing them and rotating them. Retraining a network with this transformed dataset should produce much better results for this use case.
              """
            ]
        ]
    , h3 [] [ text "Better Networks" ]
    , p [] [ text """
    The library only has standard artificial neural networks. There is no doubt that implementing more modern architectures would improve the performance. However, the aim was not to produce a state of the art deep learning library and the artificial neural networks are good enough.
    """ ]
    , h2 [] [ text "Sigmoid vs Softmax" ]
    , p []
        [ text """You may also have noted that sometimes the sum of all the predictions is greater than 100%. This is due to a sigmoid activation function being used in the output layer of the network. A softmax function could have been used which guarantees that the prediction percentages add up to 100%. However, the problem is just that, if you draw rubbish in the box it will still give you a set of predictions that adds up to 100%, not exactly what is wanted in this case. It may be interesting to do some kind of hybrid where if the total is greater than 100% softmax can be used to scale the resluts better, if not sigmoid is fine. Softmax if always a goto if you know the problem space is fully covered. This could be done by adding a 'not a number class' to the training data, however it can be a gotcha and is something to watch out for. We don't always want to force the networks to make predictions."""
        ]
    ]
