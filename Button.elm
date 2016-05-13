module Button exposing ( Button, new, view, onClick )


import Html exposing ( Html, Attribute )
import Html.Attributes as Attributes
import Html.Events as Events
import Time exposing ( Time )

import Platform.Cmd.Extra as XCmd
import Embedding exposing ( Embedding, OpaqueUpdate )


-- MODEL


type Button msg model
    = Button
        { color : { background : String
                  , text : String
                  , flash : String
                  }
        , state : State
        , delay : Time
        , embedding : Embedding (Button msg model) msg model
        }


type State
    = Flashing
    | NotFlashing


new
    :  (OpaqueUpdate msg model -> msg)
    -> ((Button msg model -> Button msg model) -> model -> model)
    -> Button msg model
new wrapOpaque liftUpdate =
    Button
        { color = { background = "#77DD77"
                  , text = "#FFFFFF"
                  , flash = "#BFFF00"
                  }
        , state = NotFlashing
        , delay = 0.5 * Time.second
        , embedding = { liftUpdate = liftUpdate
                      , wrapOpaque = wrapOpaque
                      }
        }


-- UPDATE


type Message
    = StartFlashing
    | StopFlashing


update : Message -> Button msg model -> Button msg model
update msg (Button btn) =
    case msg of
        StartFlashing ->
            { btn | state = Flashing } |> Button
        StopFlashing ->
            { btn | state = NotFlashing } |> Button


-- VIEW


view
    :  Button msg model
    -> List (Button msg model -> Attribute msg)
    -> List (Html msg)
    -> Html msg
view (Button btn) customAttrs children =
    let
        { embedding } = btn

        defaultOnClick btn =
            let
                noopMessage =
                    identity
                    |> Embedding.updateToMessage embedding []
            in
                btn |> onClick noopMessage

        backgroundColor btn =
            case btn.state of
                  Flashing ->
                      btn.color.flash
                  NotFlashing ->
                      btn.color.background

        styles (Button btn) =
            Attributes.style
                [ ( "color", btn.color.text )
                , ( "background", btn |> backgroundColor )
                , ( "font-weight", "bold" )
                ]

        defaultAttrs = [ defaultOnClick, styles ]

        allAttrs =
            defaultAttrs `List.append` customAttrs
            |> List.map (\f -> f <| Button btn)
    in
        Html.button allAttrs children


onClick : msg -> Button msg model -> Attribute msg
onClick msg (Button btn) =
    let
        { embedding } = btn

        sendDelayedMsg =
            update StopFlashing
            |> Embedding.updateToMessage embedding []
            |> XCmd.delay btn.delay

        sendUserMsg = msg |> XCmd.wrap
    in
        update StartFlashing
        |> Embedding.updateToMessage
            embedding
            [ sendUserMsg , sendDelayedMsg ]
        |> Events.onClick