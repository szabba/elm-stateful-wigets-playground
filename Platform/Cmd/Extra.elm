module Platform.Cmd.Extra exposing ( .. )


import Process
import Task
import Time exposing ( Time )


wrap : msg -> Cmd msg
wrap msg =
    Task.succeed ()
    |> Task.perform (always msg) (always msg)


delay : Time -> a -> Cmd a
delay t msg =
    Process.sleep t
    |> Task.perform (always msg) (always msg)