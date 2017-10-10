# purescript-run

[![Latest release](http://img.shields.io/github/release/natefaubion/purescript-run.svg)](https://github.com/natefaubion/purescript-run/releases)
[![Build status](https://travis-ci.org/natefaubion/purescript-run.svg?branch=master)](https://travis-ci.org/natefaubion/purescript-run)

An [extensible-effects](https://hackage.haskell.org/package/extensible-effects)
implementation for PureScript.

## Install

```
bower install purescript-run
```

## Documentation

- Module documentation is [published on Pursuit](http://pursuit.purescript.org/packages/purescript-run).

`Run` is an implementation of extensible, algebraic effects for PureScript.
This means we can write composable programs using normal PureScript data
types, and then provide interpreters for those data types when we actually
want to run them. Our effect descriptions naturally compose with others,
so we don't need to write a large encompassing data type, or explicitly
lift things through transformer stacks.

You should familiarize yourself with [purescript-variant](http://pursuit.purescript.org/packages/purescript-variant)
before using `Run`.

### Free DSLs

The `Free` data type (found in `Control.Monad.Free`) gives us a means to take
any `Functor`, and get a `Monad` instance out of it. This lets us turn fairly
simple data types into a composable DSL. Here's an example that defines a DSL
for string input and output:

```purescript
data TalkF a
  = Speak String a
  | Listen (String -> a)

derive instance functorTalkF :: Functor TalkF

type Talk = Free TalkF

-- Boilerplate definitions for lifting our constructors
-- into the Free DSL.

speak :: String -> Talk Unit
speak str = liftF (Speak str unit)

listen :: Talk String
listen = liftF (Listen id)

-- Now we can write programs using our DSL.

program :: Talk Unit
program = do
  speak $ "Hello, what is your name?"
  name <- listen
  speak $ "Nice to meet you, " <> name

```

Note that this doesn't _do_ anything yet. All we've done is define a data
type, and we can write a monadic program with it, but that program still only
exists as simple data. In order for it to do something, we'd need to provide
an interpreter which pattern matches on the data types:

```purescript
main = foldFree go program
  where
  go = case _ of
    -- Just log any speak statement
    Speak str next -> do
      Console.log str
      pure next
    -- Reply to anything with "I am Groot", but maybe
    -- we could also get input from a terminal.
    Listen reply -> do
      pure (reply "I am Groot")
```
```
Hello, what is your name?
Nice to meet you, I am Groot
```

Now say that we've written another orthogonal DSL:

```purescript
type IsThereMore = Boolean
type Bill = Int

data DinnerF a
  = Eat Food (IsThereMore -> a)
  | CheckPlease (Bill -> a)

type Dinner = Free DinnerF

-- Insert boilerplate here
```

If we could somehow combine these two data types, we could have a lovely
evening indeed. One options is to just define a new DSL which has the
capabilities of both:

```purescript
data LovelyEveningF a
  = Dining (DinnerF a)
  | Talking (TalkF a)

type LovelyEvening = Free LovelyEveningF
```

But now everytime we want to use one DSL or another, we have to explicitly
lift them into `LovelyEvening` using a natural transformation (`~>`).

```purescript
liftDinner :: Dinner ~> LovelyEvening
liftDinner = hoistFree Dining

liftTalk :: Talk ~> LovelyEvening
liftTalk = hoistFree Talking

dinnerTime :: LovelyEvening Unit
dinnerTime = do
  liftTalk $ speak "I'm famished!"
  isThereMore <- liftDinner $ eat Pizza
  if isThereMore
    then dinnerTime
    else do
      bill <- liftDinner checkPlease
      liftTalk $ speak "Outrageous!"
```

We can create these sorts of sums in a general way with `Coproduct` (`Either`
for `Functor`s):

```purescript
liftLeft :: forall f g. Free f ~> Free (Coproduct f g)
liftLeft = hoistFree left

liftRight :: forall f g. Free g ~> Free (Coproduct f g)
liftRight = hoistFree right

type LovelyEveningF = Coproduct TalkF DinnerF
type LovelyEvening = Free LovelyEveningF

dinnerTime :: LovelyEvening Unit
dinnerTime = do
  liftLeft $ speak "I'm famished!"
  isThereMore <- liftRight $ eat Pizza
  if isThereMore
    then dinnerTime
    else do
      bill <- liftRight checkPlease
      liftLeft $ speak "Outrageous!"
```

This has saved us from having to define a new composite data type, but we
still have to manually lift everywhere. And what about if we want to add
_more_ things to it? We'd need to use more and more `Coproduct`s, which
quickly gets very tedious. What if we could instead use an extensible sum
type?

`Variant` lets us encode polymorphic sum types using the row machinery in
PureScript. If we look at its big brother `VariantF` (found in
`Data.Functor.Variant`), we see that it gives us the same capability over
`Functor`s and works like an extensible `Coproduct`.

```purescript
type TALK = FProxy TalkF

_talk = SProxy :: SProxy "talk"

speak :: forall r. String -> Free (VariantF (talk :: TALK | r)) Unit
speak str = liftF (inj _talk (Speak str unit))

listen :: forall r. Free (VariantF (talk :: TALK | r)) String
listen = liftF (inj _talk (Listen id))

---

type DINNER = FProxy DinnerF

_dinner = SProxy :: SProxy "dinner"

eat :: forall r. Food -> Free (VariantF (dinner :: DINNER | r)) IsThereMore
eat food = liftF (inj _dinner (Eat food id))

checkPlease :: forall r. Free (VariantF (dinner :: DINNER | r)) Bill
checkPlease = liftF (inj _dinner (CheckPlease id))
```

Now our DSLs can be used together without any extra lifting.

```purescript
type LovelyEvening r = (dinner :: DINNER, talk :: TALK | r)

dinnerTime :: forall r. Free (VariantF (LovelyEvening r)) Unit
dinnerTime = do
  speak "I'm famished!"
  isThereMore <- eat Pizza
  if isThereMore
    then dinnerTime
    else do
      bill <- checkPlease
      speak "Outrageous!"
```

This pattern is exactly the `Run` data type:

```purescript
newtype Run r a = Run (Free (VariantF r) a)
```

In fact, this library is just a combinator zoo for writing interpreters.

### Writing Interpreters

Lets reviews our simple `TalkF` effect and example, now lifted into `Run`
instead of `Free`:

```purescript
data TalkF a
  = Speak String a
  | Listen (String -> a)

type TALK = FProxy TalkF

_talk = SProxy :: SProxy "talk"

speak :: forall r. String -> Run (talk :: TALK | r) Unit
speak str = Run.lift _talk (Speak str unit)

listen :: forall r. Run (talk :: TALK | r) String
listen = Run.lift _talk (Listen id)

program :: forall r. Run (talk :: TALK | r) Unit
program = do
  speak $ "Hello, what is your name?"
  name <- listen
  speak $ "Nice to meet you, " <> name
```

Our original `Free` based interpreter used `foldFree`, and we can do the same
thing with `Run` using `interpret` or `interpretRec`. The only difference is
that `interpretRec` uses a `MonadRec` constraint to ensure stack-safety. If
your base `Monad` is stack-safe then you don't need it and should just use
`interpret`.

Since we need to handle a `VariantF`, we need to use the combinators from
`purescript-variant`, which are re-exported by `purescript-run`.

```purescript
handleTalk :: forall eff. TalkF ~> Eff (console :: CONSOLE | eff)
handleTalk = case _ of
  Speak str next -> do
    Console.log str
    pure next
  Listen reply -> do
    pure (reply "I am Groot")

main = program # interpret (case_ # on _talk handleTalk)
```

Here we've used `case_`, which is the combinator for exhaustive pattern
matching. If we use `case_`, that means we have to provide a handler for
every effect. In this case we only have one effect, so it does the job.

Note: An alternative to `on` chaining is to use `onMatch` (or `match` for
exhaustive matching) which uses record sugar. This has really nice syntax,
but inference around polymorphic members inside of the record can be finicky,
so you might need more annotations (or eta expansion) than if you had used
`on`.

Let's try adding back in our other effect for a lovely evening:

```purescript
type DINNER = FProxy DinnerF

_dinner :: SProxy :: SProxy "dinner"

eat :: forall r. Food -> Run (dinner :: DINNER | r) IsThereMore
eat food = Run.lift _dinner (Eat food id)

checkPlease :: forall r. Run (dinner :: DINNER | r) Bill
checkPlease = Run.lift _dinner (CheckPlease id)

type LovelyEvening r = (talk :: TALK, dinner :: DINNER | r)

dinnerTime :: forall r. Run (LovelyEvening r) Unit
dinnerTime = do
  speak "I'm famished!"
  isThereMore <- eat Pizza
  if isThereMore
    then dinnerTime
    else do
      bill <- checkPlease
      speak "Outrageous!"
```

We could interpret both of these effects together in one go by providing
multiple handlers, but often times we only want to handle them one at a time.
That is, we want to interpret one effect in terms of other effects at our
convenience. We can't use `case_` then, because `case_` must always handle
all effects. Instead we can use `send` for unmatched cases.

```purescript
-- This now interprets it back into `Run` but with the `EFF` effect.
handleTalk :: forall eff r. TalkF ~> Run (eff :: EFF (console :: CONSOLE | eff) | r)
handleTalk = case _ of
  Speak str next -> do
    -- `liftEff` lifts native `Eff` effects into `Run`.
    liftEff $ Console.log str
    pure next
  Listen reply -> do
    pure (reply "I am Groot")

runTalk
  :: forall r eff
   . Run (eff :: EFF (console :: CONSOLE | eff), talk :: TALK | r)
  ~> Run (eff :: EFF (console :: CONSOLE | eff) | r)
runTalk = interpret (on _talk handleTalk send)

program2 :: forall eff r. Run (eff :: EFF (console :: CONSOLE | eff), dinner :: DINNER | r) Unit
program2 = dinnerTime # runTalk
```

We've interpreted the `TALK` effect in terms of native `Eff` effects, and so
it's no longer part of our set of `Run` effects. Instead, it has been
replaced by `EFF`. `DINNER` has yet to be interpreted, and we can choose to
do that at a later time.

In fact, let's go ahead and do that, but we will interpret it in a completely
pure manner. We will need an internal accumulator for our interpreter, which
we can do with `runAccumPure`.

```purescript
type Tally = { stock :: Int, bill :: Bill }

-- We have internal state, which is our running tally of the bill.
handleDinner :: forall a. Tally -> DinnerF a -> Tuple Tally a
handleDinner tally = case _ of
  Eat _ reply
    -- If we have food, bill the customer
    | tally.stock > 0 ->
        let tally' = { stock: tally.stock - 1, bill: tally.bill + 1 }
        in Tuple tally' (reply true)
    | otherwise ->
        Tuple tally (reply false)
  -- Reply with the bill
  CheckPlease reply ->
    Tuple tally (reply tally.bill)

-- We eliminate the `DINNER` effect altogether, yielding the result
-- together with the final bill.
runDinnerPure :: forall r a. Tally -> Run (dinner :: DINNER | r) a -> Run r (Tuple Bill a)
runDinnerPure = runAccumPure
  (\tally -> on _dinner (Loop <<< handleDinner tally) Done)
  (\tally a -> Tuple tally.bill a)

program3 :: forall r. Run (eff :: EFF (console :: CONSOLE | eff) | r) (Tuple Bill Unit)
program3 = program2 # runDinnerPure { stock: 10, bill: 0 }
```

Since both `runPure` and `runAccumPure` fully interpret their result without
running through some other `Monad` or `Run` affect, we need to preserve stack
safety using the `Step` data type from `Control.Monad.Rec.Class`. This is why
you see the `Loop` and `Done` constructors. `Loop` is used in the case of a
match, and `Done` is used in the default case.

Looking at the type of `program3`, all we have left are raw `Eff` effects.
Since `Eff` and `Aff` are the most likely target for effectful programs,
there are a few combinators for extracting them.

```purescript
program4 :: forall eff. Eff (console :: CONSOLE | eff) (Tuple Bill Unit)
program4 = runBaseEff program3
```

Additionally there are also combinators for writing interpreters via
continuation passing (`runCont`, `runAccumCont`). This is useful if you want
to just use `Eff` callbacks as your base instead of something like `Aff`.

```purescript
data LogF a = Log String a

derive instance functorLogF :: Functor LogF

type LOG = FProxy LogF

_log = SProxy :: SProxy "log"

log :: forall r. String -> Run (log :: LOG | r) Unit
log str = Run.lift _log (Log str unit)

---

data SleepF a = Sleep Int a

derive instance functorSleepF :: Functor SleepF

type SLEEP = FProxy SleepF

_sleep = SProxy :: SProxy "sleep"

sleep :: forall r. Int -> Run (sleep :: SLEEP | r) Unit
sleep ms = Run.lift _sleep (Sleep ms unit)

---

program :: forall r. Run (sleep :: SLEEP, log :: LOG | r) Unit
program = do
  log "I guess I'll wait..."
  sleep 3000
  log "I can't wait any longer!"

program2 :: forall eff. Eff (console :: CONSOLE, timer :: TIMER | eff) Unit
program2 = program # runCont go done
  where
  go = match
    { log: \(Log str cb) -> Console.log str *> cb
    , sleep: \(Sleep ms cb) -> void $ setTimeout ms cb
    }

  done _ = do
    Console.log "Done!"
```

In this case, the functor component of our effects now has the `Eff`
continuation (or callback) embedded in it, and we just invoke it to run the
rest of the program.

### Stack-safety

Since the most common target for PureScript is JavaScript, stack-safety can
be a concern. Generally, evaluating synchronous Monadic programs is not stack
safe unless your particular `Monad` of choice is designed around it. You
should use `interpretRec`, `runRec`, and `runAccumRec` if you want to
_guarantee_ stack safety in all cases, but this does come with some overhead.

Since `Run` itself is stack-safe, it's OK to use `interpret`, `run`, and
`runAccum` when interpreting an effect in terms of other `Run` effects. `Aff`
is also designed to be stack safe. `Eff`, however, is not stack safe, and you
should use the `*Rec` variations. It's not possible to guarantee stack-safety
when using the `*Cont` interpreters.
