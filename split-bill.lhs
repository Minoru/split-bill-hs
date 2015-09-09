Let's do this Literate Haskell style!

First, mandatory imports:

> import Control.Monad
> import GHC.IO.Handle
> import System.IO
>
> import Data.Map as Map

In this program, we will be asking an awful lot of questions, so let's kick off
with a function that will keep asking the question until user gives one of the
acceptable answers. Each of the acceptable answers is accompanied with a value
that will be returned if that answer is chosen:

> ask :: String -> Map Char a -> IO a
> ask question answers = do
>   putStr question
>   unless (last question == ' ') $ putStr " "
>   answer <- getChar

Now here comes a question: shouldn't we have put the `stdin` into no-buffering
mode? The answer is "yes, we *did*"; that code will reside in `main`.

>   putStrLn ""
>   case Map.lookup answer answers of
>     Just value -> return value
>     Nothing    -> ask question answers

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

Don't forget about setting (and resetting) the buffering mode for `stdin`!

> main = do
>   bufferMode <- hGetBuffering stdin
>   hSetBuffering stdin NoBuffering
>
>   mainLoop (return ())
>
>   hSetBuffering stdin bufferMode
