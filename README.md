# Today.hs

This is a very simple tool which I wrote just to test some basic haskell stuff.
I think this could help some others.

Just do `make` and then run `./today`

notice that you will need hspec to make it.
`cabal install hspec`

Have fun!


[Content from my blogpost](http://maex.me/tech/2016/06/25/writing-a-simple-cli-tool-in-haskell.html):
Hello folks,

I want to write a small tutorial on how to write a simple CLI-tool in Haskell.

For all who did not know, Haskell is a pure functional programming language which
was released in 1990. What I personally like the most is [list comprehension](https://wiki.haskell.org/List_comprehension), 
this is a really powerful and great feature. But there are also other really cool
things like [maybe](https://hackage.haskell.org/package/base-4.9.0.0/docs/Data-Maybe.html) 
for example. It is really packed with a lot of cool stuff.
i[Higher order functions](https://wiki.haskell.org/Higher_order_function), 
[currying](https://wiki.haskell.org/Currying) or 
[partial application](https://wiki.haskell.org/Partial_application) 
are very useful features too.
We will see that Haskell functions are very small, but very powerful. I've read
the sentence that you describe what a value is and not how you get to this value.
What makes it really pretty. If you want
to take a further look I really can recommend this [guide](http://learnyouahaskell.com/)
through the world of Haskell. If you want some real projects, [Facebook anti-spam 
algorithms](https://code.facebook.com/posts/745068642270222/fighting-spam-with-haskell/) are implemented in Haskell for example.


The tool we will write now will not do much, it will simply give us the current
day to our terminal.

To start you should have make(which you sure already have) [ghc - the glasgow haskell compiler](https://www.haskell.org/ghc/)
and [cabal](https://www.haskell.org/cabal/) (for a testing-library) installed.

Finally, just open a terminal.

Create a directory called today.hs. `mkdir today.hs && cd today.hs`

Create a Main.hs file and a Makefile - `touch Main.hs Makefile`.

Lets start with our Main.hs file

``` haskell
-- our main function
main :: IO ()
main = do
    putStrLn "Hello Haskell!"
```
<sup>Main.hs</sup>

To test it write `ghc -Wall Main.hs` in our terminal.
You should see something like this as output:

``` shell
$ ghc -Wall Main.hs     
[1 of 1] Compiling Main             ( Main.hs, Main.o )
Linking Main ...
```

Alright, after an ls -al you could see that there are new files called 
`Main.o`, `Main.hi` and `Main`.<br>
`Main` is the target file, the other ones are temporarily created files from the ghc
within the compile process. These can safely be deleted.
Now execute `./Main` and you should see `Hello Haskell!` on your terminal.

Because we don't always want to remember which flags and options we used let's write a makefile for that!

``` Makefile
# the compile with -Wall flags(all errors and warnings)
COMPILER = ghc -Wall 
# our main module
MAIN = Main 

# will execute other targets
all: target clean 

# depents on $(MAIN).hs - will be executed if Main.hs is changed
target: $(MAIN).hs 
        # ghc -Wall Main.hs
        $(COMPILER) $(MAIN).hs 

# cleanup
clean: 
        # remove temporary compile files
        rm $(MAIN).o $(MAIN).hi 
```
<sup>Makefile</sup>

NOTICE: you have to use tabs in the makefile or it won't work.
Now type `make` into your terminal.

``` shell
$ make
ghc -Wall Main.hs
[1 of 1] Compiling Main             ( Main.hs, Main.o )
Linking Main ...
rm Main.o Main.hi
```

Pretty cool, huh. You could see all commands and the output of the commands which
are used and we automatically get rid of the unused files.

Fine, now we are forming the "Hello Haskell!" into a function. How do we do that?
Simply so:

``` haskell
-- our main function
main :: IO ()
main = do
    putStrLn helloHaskell

-- helloHaskell returning a string
helloHaskell :: String
helloHaskell = "Hello Haskell!"
```
<sup>Main.hs</sup>

Pretty simple right?
The function name followed by `::` defines it's type signature. Every function has
a type signature. Most of the time Haskell can think of itself which type
signature would be the right. But you should always write a type signature
yourself on your functions because you could write what you are intended to do and
find errors early. If you want to see some type signatures of predefined functions
you could start ghci via `ghci`, which comes with ghc in your terminal. Then simply type `:t
function` for example the output of `:t head` is `head :: [a] -> a` This means it
takes a list([]) of a's(every type) and returns a single a(of the same type).
Lists can only contain of the same data type in Haskell, you couldn't mix chars,
integers and so on. Some functions also have type classes, for example <.
`:t (<)` is `(<) :: Ord a => a -> a -> Bool` This means a has to be of the typeclass
Ord, and the functions takes an a and an a - so two a's - and returns a bool.
2 < 3 = True. The type after the last -> is always the return value, because every
function can only return one value. If it returns functions or takes functions as
an argument you would wrap that in parentheses like map. `:t map` `map :: (a -> b) -> [a] -> [b]`
map takes a function from type a to b(they could also be the same, but don't have
to), a list of a's and returns a list of b's.

If we google for 'timestamp Haskell' we could find this module: [Data.Time.Clock.POSIX](https://hackage.haskell.org/package/time-1.6.0.1/docs/Data-Time-Clock-POSIX.html)

If we try that in the ghci with `:m Data.Time.Clock.POSIX` and type `getPOSIXTime`
we see that this is actually with some positions after the decimal point. To cut
that we can use `fmap` like : `fmap round getPOSIXTime`

``` haskell
import Data.Time.Clock.POSIX(getPOSIXTime)

-- our main function
main :: IO ()
main = do
    timestamp <- getCurrentTimestamp
    putStrLn (show timestamp)

-- returns the current timestamp
getCurrentTimestamp :: IO Integer
getCurrentTimestamp = round `fmap` getPOSIXTime
```
<sup>Main.hs</sup>

If we now compile and start our program we will see the current timestamp \o/

Alright and what next?
From the timestamp we now can calculate how many days are elapsed since the
timestamp started running. How do we do that? A day has 86400 seconds, so we
simply divide the timestamp by this number and we get the result.

``` haskell
import Data.Time.Clock.POSIX(getPOSIXTime)

-- our main function
main :: IO ()
main = do
    timestamp <- getCurrentTimestamp
    putStrLn (show $ calculateElapsedDays timestamp)

-- returns the current timestamp
getCurrentTimestamp :: IO Integer
getCurrentTimestamp = round `fmap` getPOSIXTime

-- calulate the elapsed days since 01.01.1970
calculateElapsedDays :: Integer -> Integer
calculateElapsedDays ts = ts `div` 86400
```
<sup>Main.hs</sup>

The $ is only to avoid parentheses. We could also write `putStrLn(show
(calculateElapsedDays timestamp))` But this get a bit messy if it grows.
If you are interested you can as always enter ghci and type `:t ($)` it will give you `($) :: (a -> b) -> a -> b`
$ takes af function from a to b and an a and returns a b. Look [here](http://stackoverflow.com/questions/19521246/what-does-mean-do-in-haskell).
Notice that we have used the infix operator of div, which is used with backticks,
we could also write `div ts 86400` it is the same, but for readability I prefer
the infix variant on this functions.

Because we are lazy, lets tweak our Makefile a bit. We add a target start which
will start the compiled program, so that we don't have to do this manually each
time we change something

``` Makefile
# the compile with -Wall flags(all errors and warnings)
COMPILER = ghc -Wall 
# our main module
MAIN = Main 

# will execute other targets
all: target clean 

# depents on $(MAIN).hs - will be executed if Main.hs is changed
target: $(MAIN).hs 
        # ghc -Wall Main.hs
        $(COMPILER) $(MAIN).hs 

# cleanup
clean: 
        # remove temporary compile files
        rm $(MAIN).o $(MAIN).hi 

# start
start:
        ./$(MAIN)
```
<sup>Makefile</sup>

Run make and you will see how many days are elapsed since the glorious creation
of the timestamp.

``` shell
$ make
ghc -Wall Main.hs
[1 of 1] Compiling Main             ( Main.hs, Main.o )
Linking Main ...
rm Main.o Main.hi
./Main
16977
```

Now we need a function which calculates us the current index of the day based on
the elapsed days. 

``` haskell
import Data.Time.Clock.POSIX(getPOSIXTime)

-- our main function
main :: IO ()
main = do
    timestamp <- getCurrentTimestamp
    putStrLn (show $ calculateDayIndex . calculateElapsedDays $ timestamp)

-- returns the current timestamp
getCurrentTimestamp :: IO Integer
getCurrentTimestamp = round `fmap` getPOSIXTime

-- calulate the elapsed days since 01.01.1970
calculateElapsedDays :: Integer -> Integer
calculateElapsedDays ts = ts `div` 86400

calculateDayIndex :: Integer -> Integer
calculateDayIndex x = x `mod` 7
```
<sup>Main.hs</sup>

Simple as that. The `.` is just a function composition like in maths.
`(.) :: (b -> c) -> (a -> b) -> a -> c`. It takes a function from b to
c(calculateDayIndex),
a function from a to b(calculateElapsedDays), and a(timestamp) and calculates a c -
the result. The mod function is just the modulo function.

Now run make and I get this now:

``` shell
$ make
ghc -Wall Main.hs
[1 of 1] Compiling Main             ( Main.hs, Main.o )
Linking Main ...
rm Main.o Main.hi
./Main
2
```

This will be Wednesday if we start at 0 but now is Saturday. If we do a bit
research we get that the 01.01.1970 was a Thursday, so we have to place an offset.
Adjust the function like this:

``` haskell
import Data.Time.Clock.POSIX(getPOSIXTime)

-- our main function
main :: IO ()
main = do
    timestamp <- getCurrentTimestamp
    putStrLn (show $ calculateDayIndex . calculateElapsedDays $ timestamp)

-- returns the current timestamp
getCurrentTimestamp :: IO Integer
getCurrentTimestamp = round `fmap` getPOSIXTime

-- calulate the elapsed days since 01.01.1970
calculateElapsedDays :: Integer -> Integer
calculateElapsedDays ts = ts `div` 86400

-- add 3 beacuse the 01.01.1970 was a Thursday
calculateDayIndex :: Integer -> Integer
calculateDayIndex x = (x + 3) `mod` 7
```
<sup>Main.hs</sup>

And Again:

``` shell
$ make
ghc -Wall Main.hs
[1 of 1] Compiling Main             ( Main.hs, Main.o )
Linking Main ...
rm Main.o Main.hi
./Main
5
```

Now we are at the point where we need to define a type which is representing our
days.

We will do this like this:

``` haskell
import Data.Time.Clock.POSIX(getPOSIXTime)

-- Our Day data type
data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
    deriving (Show, Enum)

-- our main function
main :: IO ()
main = do
    timestamp <- getCurrentTimestamp
    putStrLn (show $ calculateDayIndex . calculateElapsedDays $ timestamp)

-- returns the current timestamp
getCurrentTimestamp :: IO Integer
getCurrentTimestamp = round `fmap` getPOSIXTime

-- calulate the elapsed days since 01.01.1970
calculateElapsedDays :: Integer -> Integer
calculateElapsedDays ts = ts `div` 86400

-- add 3 beacuse the 01.01.1970 was a Thursday
calculateDayIndex :: Integer -> Integer
calculateDayIndex x = (x + 3) `mod` 7
```
<sup>Main.hs</sup>

We derive from the Show data type to make the days printable and the Enum data type.

Now we can write a function that gives us the current day from the index. Let's do
this.

``` haskell
import Data.Time.Clock.POSIX(getPOSIXTime)

-- Our Day data type
data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
    deriving (Show, Enum)

-- our main function
main :: IO ()
main = do
    timestamp <- getCurrentTimestamp
    putStrLn (show $ getDayFromIndex . calculateDayIndex . calculateElapsedDays $ timestamp)

-- returns the current timestamp
getCurrentTimestamp :: IO Integer
getCurrentTimestamp = round `fmap` getPOSIXTime

-- calulate the elapsed days since 01.01.1970
calculateElapsedDays :: Integer -> Integer
calculateElapsedDays ts = ts `div` 86400

-- add 3 beacuse the 01.01.1970 was a Thursday
calculateDayIndex :: Integer -> Integer
calculateDayIndex x = (x + 3) `mod` 7

-- returns a day of a given index
getDayFromIndex :: Int -> Day
getDayFromIndex x = toEnum $ fromIntegral x :: Day
```
<sup>Main.hs</sup>

And that's it, this will work but we can do a bit better.

Now we add an so called edge case to our getDayFromIndex function if it were
called with a to high or low argument, because we soon split it into modules and
we can't be sure that anybody else will use this module somewhere else.

``` haskell
import Data.Time.Clock.POSIX(getPOSIXTime)

-- Our Day data type
data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
    deriving (Show, Enum)

-- our main function
main :: IO ()
main = do
    timestamp <- getCurrentTimestamp
    putStrLn (show $ getDayFromIndex . calculateDayIndex . calculateElapsedDays $ timestamp)

-- returns the current timestamp
getCurrentTimestamp :: IO Integer
getCurrentTimestamp = round `fmap` getPOSIXTime

-- calulate the elapsed days since 01.01.1970
calculateElapsedDays :: Integer -> Integer
calculateElapsedDays ts = ts `div` 86400

-- add 3 beacuse the 01.01.1970 was a Thursday
calculateDayIndex :: Integer -> Integer
calculateDayIndex x = (x + 3) `mod` 7

-- returns a day of a given index
getDayFromIndex :: Int -> Day
getDayFromIndex x 
    | x <= 0 || x > 6 = error "Index must be between 0 and 6"
    | otherwise = toEnum $ fromIntegral x :: Day

```
<sup>Main.hs</sup>

We use so called guards(|) here. Guards are matching the arguments to a given 
pattern and return something if this case is true. So if x is lower than zero or
bigger than 6 we throw an error.

But because we are cool developer we want to split this into modules.

Create a new file called `Today.hs` and adjust it like this:

``` haskell
module Today (Day(..), getCurrentTimestamp, getDayFromTimestamp) where
import Data.Time.Clock.POSIX(getPOSIXTime)

data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
    deriving (Show, Enum)

-- Returns the current day
getDayFromTimestamp :: Integer -> Day
getDayFromTimestamp ts = getDayFromIndex . 
                            fromIntegral .
                            calculateDayIndex .
                            calculateElapsedDays $
                            ts

-- returns a day of a given index
getDayFromIndex :: Int -> Day
getDayFromIndex x
    | x < 0 || x > 6 = error "Index must be between 0 and 6"
    | otherwise = toEnum x :: Day

-- returns the current timestamp
getCurrentTimestamp :: IO Integer
getCurrentTimestamp = (round `fmap` getPOSIXTime)

-- calculate the elapsed days since 01.01.1970
calculateElapsedDays :: Integer -> Integer
calculateElapsedDays ts = ts `div` 86400


-- add 3 beacuse the 01.01.1970 was a Thursday
calculateDayIndex :: Integer -> Integer
calculateDayIndex x = (x + 3) `mod` 7
```
<sup>Today.hs</sup>

The things in parentheses after the module are the things we are exporting to
others. With the Day(..) we will export the complete Day data type.

Now we adjust our `Main.hs` file:

``` haskell
import Today (getCurrentTimestamp, getDayFromTimestamp)

main :: IO ()
main = do
    timestamp <- getCurrentTimestamp
    putStrLn 
        (show $ getDayFromTimestamp timestamp)
```
<sup>Main.hs</sup>

Of course we need to adjust our Makefile too:

``` Makefile
COMPILER = ghc -Wall

PROGNAME = Today
LOWER_PROGNAME = $(shell echo $(PROGNAME) | tr A-Z a-z)
MAIN = Main

all: target clean start

target: $(PROGNAME).hs $(MAIN).hs  
	$(COMPILER) -o $(LOWER_PROGNAME) $(MAIN).hs 

clean: 
	rm $(PROGNAME).hi $(PROGNAME).o
	rm $(MAIN).hi $(MAIN).o

start:
        ./$(LOWER_PROGNAME)
```
<sup>Makefile</sup>

Just run make and it works:

``` shell
$ make
ghc -Wall -o today Main.hs 
[1 of 2] Compiling Today            ( Today.hs, Today.o )
[2 of 2] Compiling Main             ( Main.hs, Main.o )
Linking today ...
rm Today.hi Today.o
rm Main.hi Main.o
./today
Saturday
```

You notice that we now have a binary called today and not Main as before.
So now, of course we should write tests to test our api!

I've mentioned that you need cabal installed and now we will use it.
We will use the [hspec](https://hackage.haskell.org/package/hspec) package.
So just type `cabal install hspec` and that is all.

Create a file `Test.hs`

``` haskell
import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

import Today

main :: IO ()
main = hspec $ do
    describe "Today" $ do
        it "get the right weekday from a timestamp" $ do
            getDayFromTimestamp 0 `shouldBe` Thursday
            getDayFromTimestamp 1466766758 `shouldBe` Friday
            getDayFromTimestamp 1466881750`shouldBe` Saturday
```
<sup>Test.hs</sup>

If we now run `runhaskell Test.hs` we could see an error message.

`No instance for (Eq Day) arising from a use of ‘shouldBe’` This tells us that
we have to add the Eq type class to our Day datatype, because shouldBe only works
on this kind of Types.
So we adjust our Day dataype like this:

``` haskell
module Today (Day(..), getCurrentTimestamp, getDayFromTimestamp) where
import Data.Time.Clock.POSIX(getPOSIXTime)

data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
    deriving (Eq, Show, Enum)

-- Returns the current day
getDayFromTimestamp :: Integer -> Day
getDayFromTimestamp ts = getDayFromIndex . 
                            fromIntegral .
                            calculateDayIndex .
                            calculateElapsedDays $
                            ts

-- returns a day of a given index
getDayFromIndex :: Int -> Day
getDayFromIndex x
    | x < 0 || x > 6 = error "Index must be between 0 and 6"
    | otherwise = toEnum x :: Day

-- returns the current timestamp
getCurrentTimestamp :: IO Integer
getCurrentTimestamp = (round `fmap` getPOSIXTime)

-- calculate the elapsed days since 01.01.1970
calculateElapsedDays :: Integer -> Integer
calculateElapsedDays ts = ts `div` 86400


-- add 3 beacuse the 01.01.1970 was a Thursday
calculateDayIndex :: Integer -> Integer
calculateDayIndex x = (x + 3) `mod` 7
```
<sup>Today.hs</sup>

Run `runhaskell Test.hs` again and you could see our test pass.

We want that our tests are running everytime before we compile this tool.
So back to our Makefile.

``` Makefile
COMPILER = ghc -Wall

PROGNAME = Today
LOWER_PROGNAME = $(shell echo $(PROGNAME) | tr A-Z a-z)
TEST = Test.hs
MAIN = Main

all: test target clean start

test: $(PROGNAME).hs $(MAIN).hs $(Test)
	runhaskell $(TEST)

target: $(PROGNAME).hs $(MAIN).hs  
	$(COMPILER) -o $(LOWER_PROGNAME) $(MAIN).hs 

clean: 
	rm $(PROGNAME).hi $(PROGNAME).o
	rm $(MAIN).hi $(MAIN).o

start:
        ./$(LOWER_PROGNAME)
```
<sup>Makefile</sup>

If we now run `make` we could see how our tests are running first.

``` shell
$ make
runhaskell Test.hs

Today
  get the right weekday from a timestamp

Finished in 0.0003 seconds
1 example, 0 failures
ghc -Wall -o today Main.hs 
[1 of 2] Compiling Today            ( Today.hs, Today.o )
[2 of 2] Compiling Main             ( Main.hs, Main.o )
Linking today ...
rm Today.hi Today.o
rm Main.hi Main.o
./today
Saturday
```

And that is all it takes to make a small simple CLI-tool.
I made this for me just for learning purpose but I thought others would be
interested in this too.

If you like this or have any constructive critic share this or drop me a line. 

[You could see the whole project on github as reference.](https://github.com/mstruebing/today.hs)

Thanks!
