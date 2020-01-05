module Main exposing (..)

import Browser
import Debug exposing (toString)
import Html exposing (Html, button, div, h1, text)
import Html.Attributes as HA exposing (class)
import Html.Events exposing (onClick)
import List
import String
import Svg
import Svg.Attributes as SA exposing (x1, x2, y1, y2)


main =
    Browser.document
        { init = init
        , update = update
        , subscriptions = \_ -> Sub.none
        , view = view
        }


type alias Model =
    { probs : List Float }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { probs = [] }, Cmd.none )


type Msg
    = Probs
    | NoProbs


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Probs ->
            ( { model
                | probs =
                    [ 1.3346764637908244e-15
                    , 4.979308290544717e-17
                    , 4.92448277499708e-16
                    , 5.45940372919045e-19
                    , 2.5693742985787663e-13
                    , 3.6776824920128945e-16
                    , 0.9999999999997387
                    , 4.987029384043626e-20
                    , 2.305338873271133e-15
                    , 1.798775005170958e-18
                    ]
              }
            , Cmd.none
            )

        NoProbs ->
            ( { model | probs = [] }, Cmd.none )


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
    if prob < 0.01 then
        "0%"

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


type alias Document msg =
    { title : String
    , body : List (Html msg)
    }


view : Model -> Document Msg
view model =
    { title = "Test Title"
    , body =
        [ div [ class "page" ]
            [ div [ class "heading" ] [ h1 [] [ text "Deep Haskell" ] ]
            , div [ class "content" ]
                [ div [ class "left" ]
                    [ div [ class "draw" ] []
                    , div [ class "prediction" ]
                        [ div [ class "prediction-item" ] [ text <| "Prediction: " ++ getPrediction model.probs ]
                        , div [ class "prediction-item" ] [ text <| "    Certainty: " ++ getCertainty model.probs ]
                        ]
                    , div [ class "visualise-probs" ] (visualiseProbs model.probs)
                    , button [ onClick Probs ] [ text "Get Probs" ]
                    , button [ onClick NoProbs ] [ text "Reset" ]
                    ]
                , div [ class "right" ] [ text projectInfo ]
                ]
            ]
        ]
    }


projectInfo =
    """Lorem ipsum dolor sit amet, consectetur adipiscing elit. Cras a nisi lectus. Maecenas finibus ligula a scelerisque dapibus. Morbi rutrum vel mi vitae suscipit. Fusce non erat in risus tincidunt mollis. Pellentesque non enim purus. Quisque euismod orci sed tristique congue. Suspendisse eu ligula risus. Donec sit amet magna ac libero posuere congue. Morbi nec tristique velit. Integer sit amet lectus vitae lacus imperdiet vehicula quis id massa. Fusce quis arcu quis enim iaculis pellentesque. Sed vel enim ac mauris vestibulum aliquam. Integer finibus venenatis odio vel molestie. Maecenas mattis, metus eget placerat ornare, mauris risus fringilla mi, sit amet pulvinar orci turpis sed nibh.

Aenean malesuada nisi ut pharetra feugiat. Phasellus sollicitudin commodo dignissim. Donec eget faucibus eros, nec malesuada odio. Mauris commodo efficitur pellentesque. Ut porta ac quam ut gravida. Quisque dolor magna, volutpat non convallis cursus, ornare sed neque. Suspendisse velit justo, tempor a mollis at, malesuada nec lorem. Class aptent taciti sociosqu ad litora torquent per conubia nostra, per inceptos himenaeos. Class aptent taciti sociosqu ad litora torquent per conubia nostra, per inceptos himenaeos. Nunc posuere faucibus nunc, et rutrum ante consequat in. Aliquam ac sodales nunc. Vestibulum commodo turpis egestas neque tristique, vel ultricies turpis sollicitudin.

Duis quis nisi id nisl fermentum bibendum. Curabitur laoreet, quam a condimentum pulvinar, neque justo sagittis ligula, nec dictum ex lorem a arcu. In hac habitasse platea dictumst. Aenean a venenatis ante. Nulla sit amet porttitor nulla. Nam et feugiat tortor, at dapibus quam. Praesent ornare blandit dui ac varius. Ut ultrices, libero at malesuada maximus, nulla ligula iaculis tortor, ac tempor quam ex at nibh. Donec interdum turpis sit amet urna mollis, faucibus feugiat urna feugiat. In nibh metus, aliquam vitae dolor sit amet, porta aliquam turpis. Maecenas lobortis consequat urna vitae tristique. Vestibulum non malesuada sapien. Suspendisse nibh nibh, mattis in mauris viverra, lacinia cursus purus.

Aenean metus tellus, euismod in laoreet vel, pretium non nunc. Praesent eros magna, iaculis quis magna sed, aliquet ornare velit. Nulla pharetra euismod ante. Suspendisse dapibus efficitur felis, a volutpat nisl pretium at. Pellentesque viverra libero mollis est molestie eleifend vel id ex. Aliquam in tortor cursus, volutpat est ut, vulputate ante. Maecenas molestie vitae ipsum nec ultricies. Nulla tincidunt tincidunt velit. Sed sed tempus odio, quis consequat ligula. Aenean id lacus ac orci elementum mollis. Ut vitae libero sagittis, volutpat est eu, mattis arcu. Suspendisse potenti. Nam vitae metus sed nunc aliquet hendrerit.

Maecenas eleifend semper auctor. Aliquam vitae nisi ex. Suspendisse sed velit nibh. Donec quis lobortis mi. Duis eget dictum magna, sit amet hendrerit nulla. Vestibulum ac velit egestas, placerat mi a, dictum quam. Sed lobortis, neque laoreet accumsan volutpat, lectus elit tristique diam, eu consequat risus ex sit amet nulla. Donec volutpat felis quis auctor condimentum. Sed porttitor molestie urna sed faucibus. Nulla rutrum efficitur ante, vel malesuada libero.

Vivamus ut felis faucibus mauris vehicula commodo a vitae nisl. Sed ullamcorper quam mi, in pretium dui volutpat pretium. Class aptent taciti sociosqu ad litora torquent per conubia nostra, per inceptos himenaeos. Vestibulum pellentesque diam et purus posuere, in elementum turpis bibendum. Mauris dictum aliquet enim, ut ultricies mi finibus nec. Nulla aliquam leo metus, quis iaculis mauris venenatis ac. Phasellus varius varius sem eu mollis. Pellentesque ultrices dolor diam, at vulputate risus congue vel. Aenean ac libero eu ipsum sollicitudin tincidunt sit amet vel risus. Curabitur id metus nec sapien vestibulum vestibulum. Sed mattis efficitur ultrices. Maecenas congue arcu at libero faucibus, vitae ultrices libero feugiat. Cras mi nulla, suscipit sed felis a, aliquam maximus mauris. Vivamus ac gravida odio. Nam fringilla fermentum elit luctus suscipit. Proin blandit lorem ac turpis dignissim auctor."""
