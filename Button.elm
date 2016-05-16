module Button exposing
    ( Button, new, subscriptions
    , view, Config, delay, onClick, onHover, background
    )


import AnimationFrame
import Html exposing ( Html, Attribute )
import Html.Attributes as Attributes
import Html.Events as Events
import Time exposing ( Time )

import Animation exposing ( Animation )
import Embedding exposing ( Embedding, OpaqueUpdate )


-- MODEL


type Button msg model
    = Button
        { animation : Animation State
        , subscription : Sub msg
        , embedding : Embedding (Button msg model) msg model
        }


type State
    = Flash
    | HoverFlash
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
    | StartClickFlash Time
    | Hover Time
    | Unhover


update : Message -> Button msg model -> Button msg model
update msg (Button btn) =
    let
        startAnimation anim btn =
            let
                newSubscription =
                    AnimationFrame.diffs <| \dt ->
                        Animate dt
                        |> update
                        |> Embedding.updateToMessage btn.embedding []
            in
                { btn | animation = anim, subscription = newSubscription }
                |> Button
    in
        case msg of

            StartClickFlash delay ->
                btn |> startAnimation (flash delay)

            Hover delay ->
                btn |> startAnimation (hoverFlash delay)

            Unhover ->
                btn |> startAnimation (Animation.forever NotFlashing)

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
    Animation.for (delay / 10) (Animation.forever NotFlashing)
    <| Animation.for delay (Animation.forever Flash)
    <| Animation.forever NotFlashing


hoverFlash : Time -> Animation State
hoverFlash delay  =
    Animation.for delay (Animation.forever HoverFlash)
    <| Animation.forever Flash


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


onHover : msg -> Config msg model -> Config msg model
onHover userMsg (Config cfg) =
    { cfg | onHover = wrapOnHoverMsg <| Just userMsg }
    |> Config


wrapOnClickMsg
    :  Maybe msg
    -> Config msg model
    -> Embedding (Button msg model) msg model
    -> msg
wrapOnClickMsg =
    Embedding.wrapMsg <| \(Config { delay }) -> update (StartClickFlash delay)


wrapOnHoverMsg
    :  Maybe msg
    -> Config msg model
    -> Embedding (Button msg model) msg model
    -> msg
wrapOnHoverMsg =
    Embedding.wrapMsg <| \(Config { delay }) -> update (Hover delay)


wrapOnLeaveMsg
    :  Maybe msg
    -> Config msg model
    -> Embedding (Button msg model) msg model
    -> msg
wrapOnLeaveMsg =
    Embedding.wrapMsg <| \(Config { delay }) -> update Unhover


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
        (Config { colors, onClick, onHover }) = cfg

        backgroundColor =
            case state of
                NotFlashing ->
                    colors.background
                HoverFlash ->
                    colors.hoverFlash
                Flash ->
                    colors.flash

        styles =
            Attributes.style
                [ ( "color", colors.text )
                , ( "background", backgroundColor )
                , ( "font-weight", "bold" )
                ]
    in
        [ Events.onClick <| onClick cfg embedding
        , Events.onMouseEnter <| onHover cfg embedding
        , Events.onMouseLeave <| wrapOnLeaveMsg Nothing cfg embedding
        , styles
        ]


type Config msg model
    = Config
        { colors : { background : String
                   , flash : String
                   , hoverFlash : String
                   , text : String
                   }
        , delay : Time
        , onClick
            :  Config msg model
            -> Embedding (Button msg model) msg model
            -> msg
        , onHover
            :  Config msg model
            -> Embedding (Button msg model) msg model
            -> msg
        }


defaultConfig : Config msg model
defaultConfig =
    Config
        { colors = { background = "#77DD77"
                   , text = "#FFFFFF"
                   , flash = "#BFFF00"
                   , hoverFlash = "#FFFF99"
                   }
        , delay = 0.5 * Time.second
        , onClick = wrapOnClickMsg Nothing
        , onHover = wrapOnHoverMsg Nothing
        }