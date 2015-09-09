Let's do this Literate Haskell style!

**The problem**: me and a friend of mine do our groceries shopping together, so
I've got a ton of bills that need to be accounted for in my ledger. Each item in
a bill should be categorized (food, sweets, misc expenses), and then I also need
to calculate who owes whom and how much. I did a bit of that by hand, but it's
a drudgery and I'd rather automate what I can.

This program is not meant as a generic solution for this class of problems. It
only suits my need: two persons, three accounts. But you're free to take it as
a basis for your own, if you need one.

Now let's get rolling. First, mandatory imports:

> import Control.Exception
> import Control.Monad
> import System.IO
>
> import Data.Map as Map

In this program, we will be asking an awful lot of questions, so let's kick off
with a function that will keep asking the question until user gives one of the
acceptable answers. Each of the acceptable answers is accompanied with a value
that will be returned if that answer is chosen:

> ask :: String -> Map Char a -> IO a
> ask question answers = withoutBuffering $ do
>   putStr question
>   unless (last question == ' ') $ putStr " "
>   answer <- getChar
>   putStrLn ""
>   case Map.lookup answer answers of
>     Just value -> return value
>     Nothing    -> ask question answers

`withoutBuffering` is a simple wrapper that makes sure that an action is
performed with line buffering disabled, and also resets it to its previous value
when the action is completed; `withBuffering` is a compliment function. To
define those, we first need a generic wrapper function that will set specified
buffering mode for the action:

> withBufferMode :: BufferMode -> IO a -> IO a
> withBufferMode mode action = bracket initialize finalize (const action)
>   where
>   initialize = do
>     bufferMode <- hGetBuffering stdin
>     hSetBuffering stdin mode
>     return bufferMode
>
>   finalize initialMode = hSetBuffering stdin initialMode

Now let's define our pair of wrappers:

> withBuffering :: IO a -> IO a
> withBuffering = withBufferMode LineBuffering
>
> withoutBuffering :: IO a -> IO a
> withoutBuffering = withBufferMode NoBuffering

Nice work! Let's switch gears.

The most popular type of questions is yes-no questions. Let's define an ADT for
those:

> data YesNoQuestion = Yes | No

Okay, so the first thing we need is a main loop. The idea is simple: the program
will run as long as there are bills to process. This is a do-while loop, 'cause
we're asking the question at the end, not the beginning:

> mainLoop :: IO () -> IO ()
> mainLoop action = do
>   action
>   let acceptable_answers = Map.fromList [ ( 'y', Yes ), ( 'Y', Yes ),
>                                           ( 'n', No ),  ( 'N', No ) ]
>   answer <- ask "Are there more bills to process? (y/n)" acceptable_answers
>   case answer of
>     Yes -> mainLoop action
>     No  -> return ()

> main = mainLoop (return ())
