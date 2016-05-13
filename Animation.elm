module Animation exposing
    ( Animation
    , now, runFor
    , forever, interpolate
    , for, map, andMap
    )


{-| This library lets you build and compose animations.

It's not tied to any particular graphics API, it can describe any time-evolving
values.

@docs Animation, now, runFor

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
    = Animation (Time -> (a, Animation a))


{-| Runs an animation for the given time. The results are the value after the
time has passed and the animation starting from that point onwards.

If a negative value is specified, the animation isn't actually advanced. An
animation is not a time machine.

-}
runFor : Time -> Animation a -> Animation a
runFor t (Animation consume) =
    (if t > 0 then t else 0)
    |> consume
    |> snd

{-| Retrieves the current value of the Animation.

-}
now : Animation a -> a
now (Animation consume) =
    0 |> consume |> fst


{-| Creates an animation that will always return the given value.

    runFor t (forever x) == (x, forever x)

for all `x` and `t`.

-}
forever : a -> Animation a
forever v =
    Animation <| \_ -> (v, forever v)
        -- Don't try to put `always` in here, it will cause infinite recursion.


{-| Creates an animation that's a simple function of time.

    runFor dt (interpolate f) == (f t, interpolate <| \t -> f (t + dt))

for all `f` and `dt`.

-}
interpolate : (Time -> a) -> Animation a
interpolate f =
    Animation <| \dt ->
        ( f dt
        , interpolate <| \t -> f (t + dt)
        )


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
for t (Animation consume) finally =
    Animation <| \dt ->
        if dt < t then
            let
                (now, future) = dt |> consume
                next = for (t - dt) (Animation consume) finally
            in
                ( now , next )
        else
            let (Animation consumeFinally) = finally
            in
                dt |> consumeFinally


{-| Transform the values produced by an animation with some function.

    map identity anim == anim

for all `anim`, and

    map f << map g == map (f << g)

for all `f` and `g`.

-}
map : (a -> b) -> Animation a -> Animation b
map f (Animation consume) =
    Animation <| \dt ->
        let (now, future) = dt |> consume
        in
            ( f now, map f future )


{-| Allows you to map animations over a multiple argument function.

    f `map` one `andMap` two `andMap` three `andMap` ...

-}
andMap : Animation (a -> b) -> Animation a -> Animation b
andMap (Animation consumeF) (Animation consumeX) =
    Animation <| \dt ->
        let
            (f, futureF) = dt |> consumeF
            (x, futureX) = dt |> consumeX
        in
            ( f x, andMap futureF futureX )