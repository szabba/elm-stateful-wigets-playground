module Embedding exposing ( .. )


type alias Embedding part message container =
    { wrapOpaque : OpaqueUpdate message container -> message
    , liftUpdate : (part -> part) -> container -> container
    }


type alias OpaqueUpdate msg model =
    model -> (model, Cmd msg)


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