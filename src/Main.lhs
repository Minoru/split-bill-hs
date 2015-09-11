Let's do this Literate Haskell style!

**The problem**: me and a friend of mine do our groceries shopping together, so
I've got a ton of bills that need to be accounted for in my ledger. Each item in
a bill should be categorized (food, sweets, misc expenses), and then I also need
to calculate who owes whom and how much. I did a bit of that by hand, but it's
a drudgery and I'd rather automate what I can.

This program is not meant as a generic solution for this class of problems. It
only suits my need: two persons, three accounts, one currency. But you're free
to take it as a basis for your own, if you need one.

Now let's get rolling. First, mandatory imports:

> import Control.Exception
> import Control.Monad
> import Data.Decimal
> import System.IO
>
> import Data.Map as Map

In this program, we will be asking an awful lot of questions, so let's kick off
with a function that will keep asking the question until user gives one of the
acceptable answers. Each of the acceptable answers is accompanied with a value
that will be returned if that answer is chosen (we define a map that will hold
answers coupled with it values):

> type AnswersMap a = Map Char a

> ask :: String -> AnswersMap a -> IO a
> ask question answers = withoutBuffering $ do
>   putStr question
>   unless (last question == ' ') $ putStr " "
>   hFlush stdout
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

> yesNoAnswersMap :: AnswersMap YesNoQuestion
> yesNoAnswersMap =
>   Map.fromList [ ( 'y', Yes ), ( 'Y', Yes ), ( 'n', No ),  ( 'N', No ) ]

Okay, so the first thing we need is a main loop. The idea is simple: the program
will run as long as there are bills to process. This is a do-while loop, 'cause
we're asking the question at the end, not the beginning:

> whileThereAreBills :: IO () -> IO ()
> whileThereAreBills action = do
>   action
>   answer <- ask "Are there more bills to process? (y/n)" yesNoAnswersMap
>   case answer of
>     Yes -> do
>       action
>       whileThereAreBills action
>     No  -> return ()

Great! Now let's see what we do with each bill.

First of all, we need to know who paid for it, so we know who's owing who. That
directly translated into the following type:

> data WhoPaidQuestion = Me | He
>
> whoPaidAnswersMap :: AnswersMap WhoPaidQuestion
> whoPaidAnswersMap = Map.fromList [ ('m', Me), ('h', He) ]

Then, for each item we need to know its cost, for whom it was bought (me, him,
both) and what category (food, sweets, misc) it should be counted towards. We'll
dead with the cost later; let's tackle questions first.

> data BoughtForWhomQuestion = ForMe | ForHim | ForBoth
>
> boughtForWhomAnswersMap :: AnswersMap BoughtForWhomQuestion
> boughtForWhomAnswersMap =
>   Map.fromList [ ('m', ForMe), ('h', ForHim), ('b', ForBoth) ]
>
> data CategoryQuestion = Food | Sweets | Misc
>
> categoryAnswersMap :: AnswersMap CategoryQuestion
> categoryAnswersMap =
>   Map.fromList [ ('f', Food), ('s', Sweets), ('m', Misc) ]

Right. So what's the next step? Well, obviously, while processing the items of
a bill, we need to carry around a bit of state, namely, how much money we spent
for each category, and how much we borrowed or lent. For that, let's define
a data type:

> data BillProcessingState = BillProcessingState {
>     food   :: Decimal
>   , sweets :: Decimal
>   , misc   :: Decimal
>   , lent   :: Decimal }
>
> defaultBillProcessingState = BillProcessingState 0 0 0 0

So now let's define our "main loop" for processing a bill:

> processBill :: IO ()

We won't be returning anything, 'cause the sole purpose of this function is to
have a side effect — we'll be writing to `hledger.journal`.

The function should start with asking all the questions mentioned above:

> processBill = do
>   payee <- ask "Who paid? ([m]e/[h]e)" whoPaidAnswersMap
>   state <- whileThereAreItems payee defaultBillProcessingState processItem
>   return ()
>
> processItem :: WhoPaidQuestion -> BillProcessingState
>             -> IO BillProcessingState
> processItem payee state = do
>   boughtFor <- ask "Bought for whom? ([m]e / [h]im / [b]oth)"
>                    boughtForWhomAnswersMap
>   category <- ask
>     "What category this item belongs to? ([f]ood / [s]weets / [m]isc)"
>     categoryAnswersMap
>
>   return state

> whileThereAreItems :: WhoPaidQuestion -> BillProcessingState
>                    -> (WhoPaidQuestion -> BillProcessingState
>                        -> IO BillProcessingState)
>                    -> IO BillProcessingState
> whileThereAreItems = undefined

Now let's tie it all together:

> main = whileThereAreBills processBill
