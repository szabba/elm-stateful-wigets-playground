module Animation exposing
    ( Animation
    , now, runFor, done
    , forever, interpolate
    , for, map, andMap
    )


{-| This library lets you build and compose animations.

It's not tied to any particular graphics API, it can describe any time-evolving
values.

@docs Animation, now, runFor, done

# Simple animations
@docs forever, interpolate

# Combinators
@docs for, map, andMap

-}


import Time exposing ( Time )


{-| An `Animation` describes how a value changes through time. So given an
`Animation Int` you can check how it will look after, say, a minute has passed.
Animations do not remember the past and they don't permit time travel!

-}
type Animation a
    = Animation a (Maybe (Time -> Animation a))


{-| Runs an animation for the given time. The results are the value after the
time has passed and the animation starting from that point onwards.

If a negative value is specified, the animation isn't actually advanced. An
animation is not a time machine.

-}
runFor : Time -> Animation a -> Animation a
runFor t anim =
    let (Animation _ future) = anim
    in
        case future of
            Nothing ->
                anim
            Just consumeTime ->
                if t < 0 then
                    anim
                else
                    t |> consumeTime


{-| Retrieves the current value of the Animation.

-}
now : Animation a -> a
now (Animation v _) =
    v


{-| Checks whether the animation has "settled down" -- ie, all it's future states
will be the same as the current one.

*CAUTION:* It's possible to construct an Animation that will settle down long
before `done` returns `True`. However, for every animation it will evantually
return `True`.

-}
done : Animation a -> Bool
done (Animation _ future) =
    future
    |> Maybe.map (always False)
    |> Maybe.withDefault True


{-| Creates an animation that will always return the given value.

    runFor t (forever x) == (x, forever x)

for all `x` and `t`.

-}
forever : a -> Animation a
forever v =
    Animation v Nothing
        -- Don't try to put `always` in here, it will cause infinite recursion.


{-| Creates an animation that's a simple function of time (on a bounded
interval).

-}
interpolate : Time -> (Time -> a) -> Animation a
interpolate t f =
    Animation (f 0) <| Just <| \dt ->
        if dt < t then
            interpolate (t - dt) (flip (-) dt >> f)
        else
            forever (f t)


{-| Uses the first animation for the specified time and then switches to the
second one. To build an animation pipeline use `<|` instead of `|>`:

    for Time.second (forever 3)
    <| for (30 * Time.second) (interpolate sin)
    <| forever 0

For all `first`, `second` and `t` it holds that

    runFor t (for t first second) == second

With a negative `t` you'll get

    (for t first second) == second

-}
for : Time -> Animation a -> Animation a -> Animation a
for t anim finally =
    Animation (now anim) <| Just <| \dt ->
        if dt < t then
            for (t - dt) anim finally
        else
            runFor (dt - t) finally


{-| Transform the values produced by an animation with some function.

    map identity anim == anim

for all `anim`, and

    map f << map g == map (f << g)

for all `f` and `g`.

-}
map : (a -> b) -> Animation a -> Animation b
map f anim =
    Animation (f <| now anim) <| Just <| \dt ->
        map f <| runFor dt anim


{-| Allows you to map animations over a multiple argument function.

    f `map` one `andMap` two `andMap` three `andMap` ...

-}
andMap : Animation (a -> b) -> Animation a -> Animation b
andMap (Animation f futureF) (Animation x futureX) =
    Animation (f x) <| Just <| \dt ->
        case (futureF, futureX) of

            (Nothing, Nothing) ->
                Animation (f x) Nothing

            (Just fNext, Nothing) ->
                (fNext dt) `andMap` (forever x)

            (Nothing, Just xNext) ->
                (forever f) `andMap` (xNext dt)

            (Just fNext, Just xNext) ->
                (fNext dt) `andMap` (xNext dt)