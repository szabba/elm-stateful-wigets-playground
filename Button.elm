module Button exposing
    ( Button, new, update, subscriptions
    , view, Config, delay, onClick, background
    )


import AnimationFrame
import Html exposing ( Html, Attribute )
import Html.Attributes as Attributes
import Html.Events as Events
import Time exposing ( Time )

import Animation exposing ( Animation )
import Embedding exposing ( Embedding, OpaqueUpdate )
import Platform.Cmd.Extra as XCmd


-- MODEL


type Button msg model
    = Button
        { animation : Animation State
        , subscription : Sub msg
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
        { animation = Animation.forever NotFlashing
        , subscription = Sub.none
        , embedding = { liftUpdate = liftUpdate
                      , wrapOpaque = wrapOpaque
                      }
        }


subscriptions : Button msg model -> Sub msg
subscriptions (Button { subscription }) = subscription


-- UPDATE


type Message
    = Animate Time
    | StartFlashing Time


update : Message -> Button msg model -> Button msg model
update msg (Button btn) =
    case msg of

        StartFlashing delay ->
            let
                newSubscription =
                    AnimationFrame.diffs <| \dt ->
                        Animate dt
                        |> Debug.log "animation delta"
                        |> update
                        |> Embedding.updateToMessage btn.embedding []
            in
                { btn | animation = flash delay, subscription = newSubscription }
                |> Button

        Animate dt ->
            let
                { animation, subscription } = btn

                shiftedAnimation =
                    animation
                    |> Animation.runFor dt

                newSubscription =
                    if Animation.done shiftedAnimation then
                        Sub.none
                    else
                        subscription

            in
                { btn | animation = shiftedAnimation , subscription = newSubscription }
                |> Button


flash : Time -> Animation State
flash delay =
    Animation.for delay (Animation.forever Flashing)
    <| Animation.forever NotFlashing


-- VIEW


view
    :  Button msg model
    -> List (Config msg model -> Config msg model)
    -> List (Html msg)
    -> Html msg
view (Button btn) configDelta children =
    let
        { embedding, animation } = btn

        state = animation |> Animation.now

        attributes =
            configDelta
            |> List.foldl (<|) defaultConfig
            |> attributesForConfig embedding state
    in
        Html.button attributes children


onClick : msg -> Config msg model -> Config msg model
onClick userMsg (Config cfg) =
    { cfg | onClick = wrapOnClickMsg <| Just userMsg }
    |> Config


wrapOnClickMsg
    :  Maybe msg
    -> Embedding (Button msg model) msg model
    -> Config msg model
    -> msg
wrapOnClickMsg userMsg embedding config =
    let
        (Config { delay }) = config

        sendUserMsg =
            userMsg
            |> Maybe.map XCmd.wrap
            |> Maybe.withDefault Cmd.none

        wrappedMessage =
            update (StartFlashing delay)
            |> Embedding.updateToMessage embedding [ sendUserMsg ]
    in
        wrappedMessage


delay : Time -> Config msg model -> Config msg model
delay t (Config cfg) =
    { cfg | delay = t } |> Config


background : String -> Config msg model -> Config msg model
background color (Config cfg) =
    let
        { colors } = cfg
        newColors = { colors | background = color }
    in
        { cfg | colors = newColors } |> Config


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
        , onClick = wrapOnClickMsg Nothing
        }