module Animation exposing
    ( Animation, State(..)
    , run, runState, sample, sampleState
    , isDone
    , interval, append, map
    , immediately
    )


import Time exposing ( Time )


type Animation a
    = LastStep (Step a)
    | WithPrefix (Step a) (Animation a)


type alias Step a =
    { now : Time, end : Time, f : Time -> a }


type State a
    = Done a
    | Continuing (Animation a)


run : Time -> Animation a -> State a
run t animation =
    if t < 0 then
        Continuing animation
    else
        let
            runStep ({ now, end } as step) wrapStep orElse =
                if now + t < end then
                    wrapStep { step | now = now + t }
                else
                    orElse ()
        in
            case animation of
                LastStep ({ end, f } as step) ->
                    runStep step
                        (Continuing << LastStep)
                        (\_ -> Done <| f end)

                WithPrefix ({ now, end } as step) rest ->
                    runStep step
                        (Continuing << flip WithPrefix rest)
                        (\_ -> run (t - (end - now)) rest)


runState : Time -> State a -> State a
runState dt state =
    case state of
        Done value ->
            state
        Continuing animation ->
            run dt animation


sample : Animation a -> a
sample animation =
    case animation of
        LastStep { now, f } ->
            f now
        WithPrefix { now, f } _ ->
            f now


sampleState : State a -> a
sampleState state =
    case state of
        Done value ->
            value
        Continuing animation ->
            sample animation


isDone : State a -> Bool
isDone state =
    case state of
        Done _ ->
            True
        Continuing _ ->
            False


immediately : a -> Animation a
immediately value =
    0 |> interval |> map (always value)


interval : Time -> Animation Time
interval t =
    LastStep { now = 0, end = t, f = identity }


append : Animation a -> Animation a -> Animation a
append first second =
    case first of
        LastStep step ->
            WithPrefix step second
        WithPrefix step rest ->
            WithPrefix step (rest `append` second)


map : (a -> b) -> Animation a -> Animation b
map g animation =
    let
        mapStep ({ f } as step) =
            { step | f = f >> g }
    in
        case animation of
            LastStep step ->
                LastStep <| mapStep step
            WithPrefix step rest ->
                WithPrefix (mapStep step) (map g rest)