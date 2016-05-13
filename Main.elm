module Main exposing ( .. )


import Html exposing (Html, Attribute)
import Html.App as App

import Button exposing (Button)
import Embedding exposing ( Embedding, OpaqueUpdate )


main : Program Never
main =
    App.program
        { init = (init, Cmd.none)
        , update = update
        , view = view
        , subscriptions = always Sub.none
        }


-- MODEL


type Counter
    = Counter
        { count : Int
        , upButton : Button Message Counter
        , downButton : Button Message Counter
        }


init : Counter
init =
    Counter
        { count = 0
        , upButton = Button.new Opaque upButtonL
        , downButton = Button.new Opaque downButtonL
        }


upButtonL f (Counter counter) =
    Counter { counter | upButton = f counter.upButton }


downButtonL f (Counter counter) =
    Counter { counter | downButton = f counter.downButton }


-- UPDATE


type Message
    = Decrement
    | Increment
    | Opaque (OpaqueUpdate Message Counter)


update : Message -> Counter -> (Counter, Cmd Message)
update msg model =
    let
        (Counter inner) = model
        withoutEffects = Counter >> flip (!) [ Cmd.none ]
    in
        case msg of
            Decrement ->
                { inner | count = inner.count - 1 } |> withoutEffects
            Increment ->
                { inner | count = inner.count + 1 } |> withoutEffects
            Opaque f ->
                model |> f


-- VIEW


view : Counter -> Html Message
view (Counter counter) =
    Html.div
        []
        [ Button.view counter.downButton
            [ Button.onClick Decrement ]
            [ Html.text "-" ]
        , Html.text <| toString counter.count
        , Button.view counter.upButton
            [ Button.onClick Increment ]
            [ Html.text "+" ]
        ]
