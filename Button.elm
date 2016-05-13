module Button exposing
    ( Button, new, view
    , Config, delay, onClick
    )


import Html exposing ( Html, Attribute )
import Html.Attributes as Attributes
import Html.Events as Events
import Time exposing ( Time )

import Platform.Cmd.Extra as XCmd
import Embedding exposing ( Embedding, OpaqueUpdate )


-- MODEL


type Button msg model
    = Button
        { state : State
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
        { state = NotFlashing
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
    -> List (Config msg model -> Config msg model)
    -> List (Html msg)
    -> Html msg
view (Button btn) configDelta children =
    let
        { embedding, state } = btn

        attributes =
            configDelta
            |> List.foldl (<|) defaultConfig
            |> attributesForConfig embedding state

    in
        Html.button attributes children


onClick : msg -> Config msg model -> Config msg model
onClick msg (Config cfg) =
    { cfg | onClick = \embedding cfg -> wrapOnClickMsg embedding cfg msg }
    |> Config


delay : Time -> Config msg model -> Config msg model
delay t (Config cfg ) =
    { cfg | delay = t } |> Config


wrapOnClickMsg : Embedding (Button msg model) msg model -> Config msg model -> msg -> msg
wrapOnClickMsg embedding (Config { delay }) msg =
    let
        sendUserMsg = msg |> XCmd.wrap

        sendDelayedMsg =
            update StopFlashing
            |> Embedding.updateToMessage embedding []
            |> XCmd.delay delay

        cmds = [ sendUserMsg, sendDelayedMsg ]
    in
        update StartFlashing
        |> Embedding.updateToMessage embedding cmds


attributesForConfig
    :  Embedding (Button msg model) msg model
    -> State
    -> Config msg model
    -> List (Attribute msg)
attributesForConfig embedding state cfg =
    let
        (Config { colors, onClick }) = cfg

        backgroundColor =
            case state of
                NotFlashing ->
                    colors.background
                Flashing ->
                    colors.flash

        styles =
            Attributes.style
                [ ( "color", colors.text )
                , ( "background", backgroundColor )
                , ( "font-weight", "bold" )
                ]
    in
        [ Events.onClick <| onClick embedding cfg
        , styles
        ]


type Config msg model
    = Config
        { colors : { background : String
                   , flash : String
                   , text : String
                   }
        , delay : Time
        , onClick
            :  Embedding (Button msg model) msg model
            -> Config msg model
            -> msg
        }


defaultConfig : Config msg model
defaultConfig =
    Config
        { colors = { background = "#77DD77"
                   , text = "#FFFFFF"
                   , flash = "#BFFF00"
                   }
        , delay = 0.5 * Time.second
        , onClick = \embedding cfg ->
            identity
            |> Embedding.updateToMessage embedding []
            |> wrapOnClickMsg embedding cfg
        }