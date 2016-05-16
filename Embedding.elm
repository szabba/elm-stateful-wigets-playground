module Embedding exposing ( .. )


import Platform.Cmd.Extra as XCmd


type alias Embedding part message container =
    { wrapOpaque : OpaqueUpdate message container -> message
    , liftUpdate : (part -> part) -> container -> container
    }


type alias OpaqueUpdate msg model =
    model -> (model, Cmd msg)


wrapMsg
    :  (config -> part -> part)
    -> Maybe msg
    -> config
    -> Embedding part msg model
    -> msg
wrapMsg update userMsg config embedding =
    let
        sendUserMsg =
            userMsg
            |> Maybe.map XCmd.wrap
            |> Maybe.withDefault Cmd.none

        wrappedMessage =
            update config
            |> updateToMessage embedding [ sendUserMsg ]
    in
        wrappedMessage


updateToMessage
    :  Embedding part msg model
    -> List (Cmd msg)
    -> (part -> part)
    -> msg
updateToMessage embedding cmds update =
    let
        opaqueUpdate model =
            (model |> embedding.liftUpdate update) ! cmds
    in
        opaqueUpdate |> embedding.wrapOpaque