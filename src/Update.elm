module Update exposing (fullYearOfData, getDatum, getFacts, getRandomElement, getSeed, init, justOrDefault, subs, update)

import Array exposing (Array, get)
import List exposing (append)
import Model exposing (..)
import Random exposing (Seed, initialSeed)
import String exposing (fromInt)
import Task exposing (Task, perform)
import Time exposing (now, posixToMillis)


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model Easy [] False StartGame (Random.initialSeed 0) Nothing Nothing 0 False, Cmd.none )


getDatum : Int -> Datum
getDatum i =
    justOrDefault (Array.get i fullYearOfData) ( NoCategory, 0, "Found a bug" )


{-| alternate implementation can be used if we want to have control over randon number generation
-}
prng_ : Seed -> Int -> ( Int, Seed )
prng_ seed range =
    let
        s =
            fishman20PRNG 12345

        fishman20PRNG ss =
            (ss * modBy (2 ^ 31) 48271) // 2
    in
    ( modBy range s, Random.initialSeed s )


prng : Seed -> Int -> ( Int, Seed )
prng seed range =
    Random.step (Random.int 1 (Debug.log "range" range)) seed


getFacts : Seed -> ( Seed, List Datum )
getFacts seed =
    let
        firstFactArray =
            Array.filter (\( c, y, s ) -> y > 1950 && c /= NoCategory) fullYearOfData

        ( firstFact, seed1 ) =
            getRandomElement seed firstFactArray

        ( c1, y1, s1 ) =
            justOrDefault firstFact ( NoCategory, 0, "Can't find data" )

        sameYearArray =
            Array.filter (\( c, y, s ) -> y == y1 && s /= s1 && c1 /= c && c /= NoCategory) fullYearOfData

        ( maybe, seed2 ) =
            getRandomElement seed1 (Debug.log "sameYearArray" sameYearArray)

        ( c2, y2, s2 ) =
            justOrDefault maybe ( NoCategory, 0, "Could not find two events in year " ++ fromInt y1 )

        dxYearArray =
            Array.filter (\( c, y, s ) -> (y < y1 - 10 || y > y1 + 10) && c /= c1 && c /= c2 && c /= NoCategory) fullYearOfData

        ( maybe1, seed3 ) =
            getRandomElement seed2 dxYearArray

        ( c3, y3, s3 ) =
            justOrDefault maybe1 ( NoCategory, 0, "Could not find two events in year " ++ fromInt y1 )

        dxYearArray2 =
            Array.filter (\( c, y, s ) -> (y < y3 - 10 || y > y3 + 10) && c /= c3) dxYearArray

        ( maybe2, seed4 ) =
            getRandomElement seed3 dxYearArray2

        dx =
            justOrDefault maybe2 ( NoCategory, 0, "" ++ fromInt y1 )
    in
    ( seed3, [ ( c1, y1, s1 ), ( c2, y2, s2 ), ( c3, y3, s3 ), dx ] )


getManyFacts : Int -> ( Seed, List Datum ) -> ( Seed, List Datum )
getManyFacts n ( seed, data ) =
    let
        ( newSeed, newDatum ) =
            getFacts seed

        shuffled =
            shuffle seed newDatum
    in
    if n < 1 then
        ( newSeed, data )

    else
        getManyFacts (n - 1) ( newSeed, append data shuffled )


getRandomElement : Seed -> Array a -> ( Maybe a, Seed )
getRandomElement seed array =
    let
        ( i, seed1 ) =
            prng seed (Array.length array - 1)
    in
    ( Array.get i array, seed1 )


getSeed : Model -> Seed
getSeed model =
    justOrDefault model.randomSeed (Random.initialSeed 0)


justOrDefault : Maybe a -> a -> a
justOrDefault maybe default =
    Maybe.withDefault default maybe


subs : Model -> Sub Msg
subs model =
    Sub.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Start ->
            ( { model
                | isHidden = True
                , mode = StartGame
                , started = True
              }
            , Random.generate FirstRoll (Random.int 0 Random.maxInt)
            )

        PrintAnswers ->
            let
                printSeed =
                    model.printSeed

                ( seed1, facts ) =
                    getManyFacts 20 ( printSeed, [] )
            in
            ( { model
                | isHidden = False
                , mode = ShowManyAnswers
                , printSeed = printSeed
                , randomSeed = Just seed1
                , facts = facts
              }
            , Cmd.none
            )

        PrintQuiz ->
            let
                printSeed =
                    justOrDefault model.randomSeed (Random.initialSeed 0)

                ( seed1, facts ) =
                    getManyFacts 20 ( printSeed, [] )
            in
            ( { model
                | isHidden = True
                , facts = facts
                , mode = ShowManyQuestions
                , randomSeed = Just seed1
                , printSeed = printSeed
                , started = True
              }
            , Cmd.none
            )

        Roll ->
            let
                ( seed1, facts ) =
                    getSeed model |> getFacts
            in
            ( { model
                | facts =
                    if model.difficulty == Easy then
                        List.take 3 facts

                    else
                        facts
                , isHidden = True
                , randomSeed = Just seed1
              }
            , Cmd.none
            )

        FirstRoll newFace ->
            let
                ( seed, facts ) =
                    Random.initialSeed newFace |> getFacts
            in
            ( { model
                | sort = newFace
                , facts =
                    if model.difficulty == Easy then
                        List.take 3 facts

                    else
                        facts
                , isHidden = True
                , mode = PlayGame
                , randomSeed = Just seed
              }
            , Cmd.none
            )

        Reveal ->
            ( { model
                | isHidden = False
              }
            , Cmd.none
            )

        ToggleDifficulty ->
            ( { model
                | difficulty =
                    if model.difficulty == Easy then
                        Hard

                    else
                        Easy
              }
            , Cmd.none
            )

        CloseWelcomeScreen ->
            ( { model
                | mode = StartGame
              }
            , Task.perform StartApp Time.now
            )

        StartApp time ->
            ( { model
                | startTime = Just time
                , randomSeed = Just (initialSeed (posixToMillis time))
              }
            , Cmd.none
            )


fullYearOfData =
    Array.fromList
        [ ( NoCategory, 0, "dummydumdum" )
        , ( Building, 1930, "The Chrysler Building became the tallest building in the world. " )
        , ( Building, 1931, "The Empire State Building became the tallest building in the world. " )
        , ( Building, 1972, "The World Trade Center became the tallest building in the world. " )
        , ( Building, 1974, "The Sears Tower became the tallest building in the world. " )
        , ( Building, 1998, "The Petronas Towers became the tallest building in the world. " )
        , ( Building, 2004, "The Taipei 101 became the tallest building in the world. " )
        , ( Building, 2009, "The Burj Khalifa became the tallest building in the world. " )
        , ( Sports, 1964, "Muhammad Ali as Cassius Clay became the undisputed heavyweight boxing champion" )
        , ( Sports, 1970, "Joe Frazier became the undisputed heavyweight boxing champion" )
        , ( Sports, 1973, "George Foreman became the undisputed heavyweight boxing champion" )
        , ( Sports, 1978, "Leon Spinks became the undisputed heavyweight boxing champion" )
        , ( Sports, 1987, "Mike Tyson became the undisputed heavyweight boxing champion" )
        , ( Sports, 1990, "James \"Buster\" Douglas became the undisputed heavyweight boxing champion" )
        , ( Sports, 1990, "Evander Holyfield became the undisputed heavyweight boxing champion" )
        , ( Sports, 1992, "Riddick Bowe became the undisputed heavyweight boxing champion" )
        , ( Sports, 1999, "Lennox Lewis became the undisputed heavyweight boxing champion" )
        , ( FIFA, 1930, "Uruguay hosted the FIFA World Cup" )
        , ( FIFA, 1954, "Switzerland hosted the FIFA World Cup" )
        , ( FIFA, 1958, "Sweden hosted the FIFA World Cup" )
        , ( FIFA, 1962, "Chile hosted the FIFA World Cup" )
        , ( FIFA, 1966, "England hosted the FIFA World Cup" )
        , ( FIFA, 1974, "Argentina hosted the FIFA World Cup" )
        , ( FIFA, 1982, "Spain hosted the FIFA World Cup" )
        , ( FIFA, 1994, "USA hosted the FIFA World Cup" )
        , ( FIFA, 2002, "Japan and South Korea hosted the FIFA World Cup" )
        , ( FIFA, 2010, "South Africa hosted the FIFA World Cup" )
        , ( Olympics, 1912, "Sweden hosted the Summer Olympics" )
        , ( Olympics, 1920, "Belgium hosted the Summer Olympics" )
        , ( Olympics, 1928, "Netherlands hosted the Summer Olympics" )
        , ( Olympics, 1968, "Mexico hosted the Summer Olympics" )
        , ( Olympics, 1984, "SFR Yugoslavia hosted the Summer Olympics" )
        , ( Olympics, 1994, "Spain hosted the Summer Olympics" )
        , ( Olympics, 2016, "Brazil hosted the Summer Olympics" )
        , ( Politics, 1912, "Theodore Roosevelt lost the presidential election" )
        , ( Politics, 1932, "Herbert Hoover lost the presidential election" )
        , ( Politics, 1940, "Wendell L. Willkie lost the presidential election" )
        , ( Politics, 1944, "Thomas E. Dewey lost the presidential election" )
        , ( Politics, 1960, "Richard M. Nixon lost the presidential election" )
        , ( Politics, 1964, "Barry M. Goldwater lost the presidential election" )
        , ( Politics, 1968, "Hubert H. Humphrey lost the presidential election" )
        , ( Politics, 1972, "George S. McGovern lost the presidential election" )
        , ( Politics, 1976, "Gerald R. Ford lost the presidential election" )
        , ( Politics, 1980, "Jimmy Carter lost the presidential election" )
        , ( Politics, 1984, "Walter F. Mondale lost the presidential election" )
        , ( Politics, 1988, "Michael Dukakis lost the presidential election" )
        , ( Politics, 1992, "George (Herbert Walker) Bush lost the presidential election" )
        , ( Politics, 1996, "Robert J. Dole lost the presidential election" )
        , ( Politics, 2000, "Al Gore lost the presidential election" )
        , ( Politics, 2004, "John Kerry lost the presidential election" )
        , ( Politics, 2008, "John McCain lost the presidential election" )
        , ( Politics, 2012, "Mitt Romney lost the presidential election" )
        , ( Music, 2014, "Taylor Swift released the year's highest grossing album \"1989\"" )
        , ( Music, 2011, "Adele released the year's highest grossing album \"21\"" )
        , ( Music, 2015, "Adele released the year's highest grossing album \"25\"" )
        , ( Music, 1979, "Billy Joel released the year's highest grossing album \"52nd Street\"" )
        , ( Music, 1968, "The Jimi Hendrix Experience released the year's highest grossing album \"Are You Experienced?\"" )
        , ( Music, 1982, "Asia released the year's highest grossing album \"Asia\"" )
        , ( Music, 1985, "Bruce Springsteen released the year's highest grossing album \"Born in the U.S.A.\"" )
        , ( Music, 1970, "Simon and Garfunkel released the year's highest grossing album \"Bridge over Troubled Water\"" )
        , ( Music, 2004, "Usher released the year's highest grossing album \"Confessions\"" )
        , ( Music, 1995, "Hootie and the Blowfish released the year's highest grossing album \"Cracked Rear View\"" )
        , ( Music, 1989, "Bobby Brown released the year's highest grossing album \"Don't Be Cruel\"" )
        , ( Music, 1988, "George Michael released the year's highest grossing album \"Faith\"" )
        , ( Music, 2009, "Taylor Swift released the year's highest grossing album \"Fearless\"" )
        , ( Music, 1976, "Peter Frampton released the year's highest grossing album \"Frampton Comes Alive\"" )
        , ( Music, 2003, "50 Cent released the year's highest grossing album \"Get Rich or Die Tryin'\"" )
        , ( Music, 1974, "Elton John released the year's highest grossing album \"Goodbye Yellow Brick Road\"" )
        , ( Music, 1972, "Neil Young released the year's highest grossing album \"Harvest\"" )
        , ( Music, 1981, "REO Speedwagon released the year's highest grossing album \"Hi Infidelity\"" )
        , ( Music, 2001, "Linkin Park released the year's highest grossing album \"Hybrid Theory\"" )
        , ( Music, 1969, "Iron Butterfly released the year's highest grossing album \"In-A-Gadda-Da-Vida\"" )
        , ( Music, 1996, "Alanis Morissette released the year's highest grossing album \"Jagged Little Pill\"" )
        , ( Music, 1990, "Janet Jackson released the year's highest grossing album \"Janet Jackson's Rhythm Nation 1814\"" )
        , ( Music, 1991, "Mariah Carey released the year's highest grossing album \"Mariah Carey\"" )
        , ( Music, 1999, "Backstreet Boys released the year's highest grossing album \"Millennium\"" )
        , ( Music, 1967, "The Monkees released the year's highest grossing album \"More of The Monkees\"" )
        , ( Music, 2000, "NSYNC released the year's highest grossing album \"No Strings Attached\"" )
        , ( Music, 2007, "Josh Groban released the year's highest grossing album \"Noël\"" )
        , ( Music, 2010, "Eminem released the year's highest grossing album \"Recovery\"" )
        , ( Music, 1977, "Fleetwood Mac released the year's highest grossing album \"Rumours\"" )
        , ( Music, 1978, "Bee Gees released the year's highest grossing album \"Saturday Night Fever\"" )
        , ( Music, 1987, "Bon Jovi released the year's highest grossing album \"Slippery When Wet\"" )
        , ( Music, 1992, "Billy Ray Cyrus released the year's highest grossing album \"Some Gave All\"" )
        , ( Music, 1997, "Spice Girls released the year's highest grossing album \"Spice\"" )
        , ( Music, 2008, "Lil Wayne released the year's highest grossing album \"Tha Carter III\"" )
        , ( Music, 2013, "Justin Timberlake released the year's highest grossing album \"The 20/20 Experience\"" )
        , ( Music, 2005, "Mariah Carey released the year's highest grossing album \"The Emancipation of Mimi\"" )
        , ( Music, 2002, "Eminem released the year's highest grossing album \"The Eminem Show\"" )
        , ( Music, 1994, "Elton John/soundtrack released the year's highest grossing album \"The Lion King\"" )
        , ( Music, 1980, "Pink Floyd released the year's highest grossing album \"The Wall\"" )
        , ( Music, 1973, "War released the year's highest grossing album \"The World Is a Ghetto\"" )
        , ( Music, 1983, "Michael Jackson released the year's highest grossing album \"Thriller\"" )
        , ( Music, 1966, "Herb Alpert & The Tijuana Brass released the year's highest grossing album \"Whipped Cream & Other Delights\"" )
        , ( Music, 1986, "Whitney Houston released the year's highest grossing album \"Whitney Houston\"" )
        , ( Hurricane, 1969, "Hurricane Camille struck the United States" )
        , ( Hurricane, 1989, "Hurricane Hugo struck the United States" )
        , ( Hurricane, 1992, "Hurricane Andrew struck the United States" )
        , ( Hurricane, 2005, "Hurricane Katrina struck the United States" )
        , ( Hurricane, 2008, "Hurricane Ike struck the United States" )
        , ( Hurricane, 2012, "Hurricane Sandy struck the United States" )
        , ( Politics, 2011, "South Sudan was founded as a nation" )
        , ( Movie, 1937, "Snow White and the Seven Dwarfs was the highest grossing movie of the year" )
        , ( Movie, 1939, "Gone with the Wind was the highest grossing movie of the year" )
        , ( Movie, 1956, "The Ten Commandments was the highest grossing movie of the year" )
        , ( Movie, 1960, "Psycho was the highest grossing movie of the year" )
        , ( Movie, 1961, "West Side Story was the highest grossing movie of the year" )
        , ( Movie, 1965, "Doctor Zhivago was the highest grossing movie of the year" )
        , ( Movie, 1965, "The Sound of Music was the highest grossing movie of the year" )
        , ( Movie, 1967, "The Graduate was the highest grossing movie of the year" )
        , ( Movie, 1968, "2001: A Space Odyssey was the highest grossing movie of the year" )
        , ( Movie, 1969, "Butch Cassidy and the Sundance Kid was the highest grossing movie of the year" )
        , ( Movie, 1972, "The Godfather was the highest grossing movie of the year" )
        , ( Movie, 1973, "The Exorcist was the highest grossing movie of the year" )
        , ( Movie, 1974, "Blazing Saddles was the highest grossing movie of the year" )
        , ( Movie, 1975, "Jaws was the highest grossing movie of the year" )
        , ( Movie, 1975, "Jaws was the highest grossing movie of the year" )
        , ( Movie, 1976, "Rocky was the highest grossing movie of the year" )
        , ( Movie, 1977, "Star Wars was the highest grossing movie of the year" )
        , ( Movie, 1978, "Grease was the highest grossing movie of the year" )
        , ( Movie, 1981, "Raiders of the Lost Ark was the highest grossing movie of the year" )
        , ( Movie, 1982, "E.T. the Extra-Terrestrial was the highest grossing movie of the year" )
        , ( Movie, 1985, "Back to the Future was the highest grossing movie of the year" )
        , ( Movie, 1988, "Rain Man was the highest grossing movie of the year" )
        , ( Movie, 1992, "Aladdin was the highest grossing movie of the year" )
        , ( Movie, 1993, "Jurassic Park was the highest grossing movie of the year" )
        , ( Movie, 1994, "The Lion King was the highest grossing movie of the year" )
        , ( Movie, 1996, "Independence Day was the highest grossing movie of the year" )
        , ( Movie, 1997, "Titanic was the highest grossing movie of the year" )
        , ( Movie, 2001, "The Lord of the Rings: The Fellowship of the Ring was the highest grossing movie of the year" )
        , ( Movie, 2003, "Finding Nemo was the highest grossing movie of the year" )
        , ( Movie, 2008, "The Dark Knight was the highest grossing movie of the year" )
        , ( Movie, 2009, "Avatar was the highest grossing movie of the year" )
        , ( Movie, 2012, "The Hobbit: An Unexpected Journey was the highest grossing movie of the year" )
        , ( Movie, 2013, "The Hunger Games: Catching Fire was the highest grossing movie of the year" )
        , ( Movie, 2015, "Jurassic World was the highest grossing movie of the year" )
        , ( Movie, 2015, "Minions was the highest grossing movie of the year" )
        , ( Movie, 2016, "Batman v Superman: Dawn of Justice was the highest grossing movie of the year" )
        , ( Sports, 1951, "Rochester Royals won its only NBA championship" )
        , ( Sports, 1955, "Syracuse Nationals won its only NBA championship" )
        , ( Sports, 1958, "St. Louis Hawks won its only NBA championship" )
        , ( Sports, 1971, "Milwaulkee Bucks won its only NBA championship" )
        , ( Sports, 1977, "Portland Trailblazers won its only NBA championship" )
        , ( Sports, 1978, "Washington Bullets won its only NBA championship" )
        , ( Sports, 1979, "Seattle Supersonics won its only NBA championship" )
        , ( Sports, 2011, "Dallas Mavericks won its only NBA championship" )
        , ( Sports, 2016, "Cleveland Cavaliers won its only NBA championship" )
        , ( Sports, 1968, "New York Jets won its only Super Bowl" )
        , ( Sports, 1970, "Kansas City Chiefs won its only Super Bowl" )
        , ( Sports, 1986, "Chicago Bears won its only Super Bowl" )
        , ( Sports, 2000, "St Louis Rams won its only Super Bowl" )
        , ( Sports, 2003, "Tampa Bay Buccaneers won its only Super Bowl" )
        , ( Sports, 2010, "New Orleans Saints won its only Super Bowl" )
        , ( Sports, 2014, "Seattle Seahawks won its only Super Bowl" )
        , ( Sports, 1924, "Washington Senators won its only World Series Championship" )
        , ( Sports, 1955, "Brooklyn Dodgers won its only World Series Championship" )
        , ( Sports, 1957, "Milwaulkee Braves won its only World Series Championship" )
        , ( Sports, 1995, "Atlanta Braves won its only World Series Championship" )
        , ( Sports, 2001, "Arizona Diamondbacks won its only World Series Championship" )
        , ( Sports, 2002, "Anaheim Angels won its only World Series Championship" )
        , ( FIFA, 1966, "England won the Fifa World Cup" )
        , ( FIFA, 1978, "Argentina won the Fifa World Cup" )
        , ( FIFA, 1998, "France won the Fifa World Cup" )
        , ( Movie, 1954, "On the Waterfront won the Oscar for Best Picture" )
        , ( Movie, 1956, "Around the World in 80 Days won the Oscar for Best Picture" )
        , ( Movie, 1957, "The Bridge on the River Kwai won the Oscar for Best Picture" )
        , ( Movie, 1959, "Ben-Hur won the Oscar for Best Picture" )
        , ( Movie, 1960, "The Apartment won the Oscar for Best Picture" )
        , ( Movie, 1961, "West Side Story won the Oscar for Best Picture" )
        , ( Movie, 1962, "Lawrence of Arabia won the Oscar for Best Picture" )
        , ( Movie, 1964, "My Fair Lady won the Oscar for Best Picture" )
        , ( Movie, 1965, "The Sound of Music won the Oscar for Best Picture" )
        , ( Movie, 1969, "Midnight Cowboy won the Oscar for Best Picture" )
        , ( Movie, 1971, "The French Connection won the Oscar for Best Picture" )
        , ( Movie, 1972, "The Godfather won the Oscar for Best Picture" )
        , ( Movie, 1973, "The Sting won the Oscar for Best Picture" )
        , ( Movie, 1974, "The Godfather Part II won the Oscar for Best Picture" )
        , ( Movie, 1975, "One Flew over the Cuckoo's Nest won the Oscar for Best Picture" )
        , ( Movie, 1976, "Rocky won the Oscar for Best Picture" )
        , ( Movie, 1977, "Annie Hall won the Oscar for Best Picture" )
        , ( Movie, 1978, "The Deer Hunter won the Oscar for Best Picture" )
        , ( Movie, 1979, "Kramer vs. Kramer won the Oscar for Best Picture" )
        , ( Movie, 1980, "Ordinary People won the Oscar for Best Picture" )
        , ( Movie, 1981, "Chariots of Fire won the Oscar for Best Picture" )
        , ( Movie, 1982, "Gandhi won the Oscar for Best Picture" )
        , ( Movie, 1983, "Terms of Endearment won the Oscar for Best Picture" )
        , ( Movie, 1984, "Amadeus won the Oscar for Best Picture" )
        , ( Movie, 1986, "Platoon won the Oscar for Best Picture" )
        , ( Movie, 1988, "Rain Man won the Oscar for Best Picture" )
        , ( Movie, 1989, "Driving Miss Daisy won the Oscar for Best Picture" )
        , ( Movie, 1990, "Dances With Wolves won the Oscar for Best Picture" )
        , ( Movie, 1991, "The Silence of the Lambs won the Oscar for Best Picture" )
        , ( Movie, 1992, "Unforgiven won the Oscar for Best Picture" )
        , ( Movie, 1993, "Schindler’s List won the Oscar for Best Picture" )
        , ( Movie, 1994, "Forrest Gump won the Oscar for Best Picture" )
        , ( Movie, 1995, "Braveheart won the Oscar for Best Picture" )
        , ( Movie, 1996, "The English Patient won the Oscar for Best Picture" )
        , ( Movie, 1997, "Titanic won the Oscar for Best Picture" )
        , ( Movie, 2000, "Gladiator won the Oscar for Best Picture" )
        , ( Movie, 2001, "A Beautiful Mind won the Oscar for Best Picture" )
        , ( Movie, 2002, "Chicago won the Oscar for Best Picture" )
        , ( Movie, 2003, "The Lord of the Rings: The Return of the King won the Oscar for Best Picture" )
        , ( Movie, 2004, "Million Dollar Baby won the Oscar for Best Picture" )
        , ( Movie, 2007, "No Country for Old Men won the Oscar for Best Picture" )
        , ( Movie, 2008, "Slumdog Millionaire won the Oscar for Best Picture" )
        , ( Movie, 2009, "The Hurt Locker won the Oscar for Best Picture" )
        , ( Movie, 2010, "The King's Speech won the Oscar for Best Picture" )
        , ( Movie, 2011, "The Artist won the Oscar for Best Picture" )
        , ( Movie, 2012, "Argo won the Oscar for Best Picture" )
        , ( Sports, 1919, "Sir Barton won the Triple Crown" )
        , ( Sports, 1930, "Gallant Fox won the Triple Crown" )
        , ( Sports, 1935, "Omaha won the Triple Crown" )
        , ( Sports, 1937, "War Admiral won the Triple Crown" )
        , ( Sports, 1941, "Whirlaway won the Triple Crown" )
        , ( Sports, 1943, "Count Fleet won the Triple Crown" )
        , ( Sports, 1946, "Assault won the Triple Crown" )
        , ( Sports, 1948, "Citation won the Triple Crown" )
        , ( Sports, 1973, "Secretariat won the Triple Crown" )
        , ( Sports, 1977, "Seattle Slew won the Triple Crown" )
        , ( Sports, 1978, "Affirmed won the Triple Crown" )
        , ( Sports, 2015, "American Pharoah won the Triple Crown" )
        , ( Tv, 1951, "The television series \"I Love Lucy\" premiered and would run for 6 years." )
        , ( Tv, 1959, "The television series \"The Twilight Zone\" premiered and would run for 5 years." )
        , ( Tv, 1960, "The television series \"The Flinstones\" premiered and would run for 6 years." )
        , ( Tv, 1961, "The television series \"The Dick Van Dyke Show\" premiered and would run for 5 years." )
        , ( Tv, 1964, "The television series \"Gilligan's Islan\" premiered and would run for 3 years." )
        , ( Tv, 1966, "The television series \"Star Trek \" premiered and would run for 3 years." )
        , ( Tv, 1965, "The television series \"Get Smart\" premiered and would run for 5 years." )
        , ( Tv, 1969, "The television series \"The Brady Bunch\" premiered and would run for 5 years." )
        , ( Tv, 1969, "The television series \"Monty Python's Flying Circus\" premiered and would run for 5 years." )
        , ( Tv, 1970, "The television series \"The Mary Tyler Moore Show\" premiered and would run for 7 years." )
        , ( Tv, 1972, "The television series \"The Bob Newhart Show\" premiered and would run for 6 years." )
        , ( Tv, 1971, "The television series \"All in the Family\" premiered and would run for 8 years." )
        , ( Tv, 1974, "The television series \"Rockford Files\" premiered and would run for 6 years." )
        , ( Tv, 1978, "The television series \"Mork & Mindy\" premiered and would run for 4 years." )
        , ( Tv, 1978, "The television series \"Taxi\" premiered and would run for 5 years." )
        , ( Tv, 1972, "The television series \"M*A*S*H\" premiered and would run for 11 years." )
        , ( Tv, 1981, "The television series \"Hill Street Blues\" premiered and would run for 6 years." )
        , ( Tv, 1982, "The television series \"Family Ties\" premiered and would run for 7 years." )
        , ( Tv, 1985, "The television series \"Moonlighting\" premiered and would run for 4 years." )
        , ( Tv, 1990, "The television series \"Twin Peaks\" premiered and would run for 1 years." )
        , ( Tv, 1985, "The television series \"The Golden Girls\" premiered and would run for 7 years." )
        , ( Tv, 1979, "The television series \"Saved by the Bell\" premiered and would run for 14 years." )
        , ( Tv, 1988, "The television series \"The Wonder Years\" premiered and would run for 5 years." )
        , ( Tv, 1982, "The television series \"Cheers\" premiered and would run for 11 years." )
        , ( Tv, 1987, "The television series \"Star Trek: The Next Generation\" premiered and would run for 7 years." )
        , ( Tv, 1987, "The television series \"Full House\" premiered and would run for 8 years." )
        , ( Tv, 1990, "The television series \"The Fresh Prince of Bel-Air\" premiered and would run for 6 years." )
        , ( Tv, 1987, "The television series \"Married.. With Children\" premiered and would run for 10 years." )
        , ( Tv, 1988, "The television series \"Roseanne\" premiered and would run for 9 years." )
        , ( Tv, 1988, "The television series \"Murphy Brown\" premiered and would run for 10 years." )
        , ( Tv, 1989, "The television series \"Seinfeld\" premiered and would run for 9 years." )
        , ( Tv, 1997, "The television series \"Ally McBeal\" premiered and would run for 5 years." )
        , ( Tv, 1993, "The television series \"The X-Files\" premiered and would run for 9 years." )
        , ( Tv, 2002, "The television series \"Firefly\" premiered and would run for 1 years." )
        , ( Tv, 1997, "The television series \"Buffy the Vampire Slayer\" premiered and would run for 6 years." )
        , ( Tv, 1993, "The television series \"Frasier\" premiered and would run for 11 years." )
        , ( Tv, 1998, "The television series \"Sex and the City\" premiered and would run for 6 years." )
        , ( Tv, 1994, "The television series \"Friends\" premiered and would run for 10 years." )
        , ( Tv, 1996, "The television series \"Everybody Loves Raymond\" premiered and would run for 9 years." )
        , ( Tv, 2001, "The television series \"Six Feet Under\" premiered and would run for 4 years." )
        , ( Tv, 1999, "The television series \"The West Wing\" premiered and would run for 7 years." )
        , ( Tv, 2000, "The television series \"Gilmore Girls\" premiered and would run for 7 years." )
        , ( Tv, 1999, "The television series \"The Sopranos\" premiered and would run for 8 years." )
        , ( Tv, 2004, "The television series \"Battlestar Galactica\" premiered and would run for 5 years." )
        , ( Tv, 1994, "The television series \"ER\" premiered and would run for 15 years." )
        , ( Tv, 2001, "The television series \"Scrubs\" premiered and would run for 9 years." )
        , ( Tv, 1990, "The television series \"Law and Order\" premiered and would run for 20 years." )
        , ( Tv, 2004, "The television series \"Lost\" premiered and would run for 6 years." )
        , ( Tv, 2004, "The television series \"Entourage\" premiered and would run for 7 years." )
        , ( Tv, 2004, "The television series \"House\" premiered and would run for 8 years." )
        , ( Tv, 2006, "The television series \"Dexter\" premiered and would run for 7 years." )
        , ( Tv, 2005, "The television series \"The Office\" premiered and would run for 8 years." )
        , ( Tv, 2006, "The television series \"30 Rock\" premiered and would run for 7 years." )
        , ( Tv, 2008, "The television series \"Breaking Bad\" premiered and would run for 5 years." )
        , ( Tv, 2008, "The television series \"True Blood\" premiered and would run for 6 years." )
        , ( Tv, 2005, "The television series \"How I Met Your Mother\" premiered and would run for 9 years." )
        , ( Tv, 2009, "The television series \"Parks and Recreation\" premiered and would run for 6 years." )
        , ( Tv, 2007, "The television series \"Mad Men\" premiered and would run for 8 years." )
        , ( Tv, 1969, "The television series \"Sesame Street\" premiered." )
        , ( Tv, 1999, "The television series \"Family Guy\" premiered." )
        , ( Tv, 2010, "The television series \"Downton Abbey\" premiered." )
        , ( Tv, 1997, "The television series \"South Park\" premiered." )
        , ( Tv, 2013, "The television series \"Orange Is the New Black\" premiered." )
        , ( Tv, 2007, "The television series \"The Big Bang Theory\" premiered." )
        , ( Tv, 2012, "The television series \"The Walking Dead\" premiered." )
        , ( Tv, 2013, "The television series \"House of Cards\" premiered." )
        , ( Tv, 1998, "The television series \"Will & Grace\" premiered." )
        , ( Tv, 2009, "The television series \"Modern Family\" premiered." )
        , ( Tv, 1989, "The television series \"The Simpsons\" premiered." )
        , ( Tv, 1975, "The television series \"Saturday Night Live\" premiered." )
        , ( Tv, 2011, "The television series \"Game of Thrones\" premiered." )
        , ( Tv, 1957, "The finale of \"I Love Lucy\" aired, ending a 6 year run." )
        , ( Tv, 1964, "The finale of \"The Twilight Zone\" aired, ending a 5 year run." )
        , ( Tv, 1966, "The finale of \"The Flinstones\" aired, ending a 6 year run." )
        , ( Tv, 1966, "The finale of \"The Dick Van Dyke Show\" aired, ending a 5 year run." )
        , ( Tv, 1967, "The finale of \"Gilligan's Islan\" aired, ending a 3 year run." )
        , ( Tv, 1969, "The finale of \"Star Trek \" aired, ending a 3 year run." )
        , ( Tv, 1970, "The finale of \"Get Smart\" aired, ending a 5 year run." )
        , ( Tv, 1974, "The finale of \"The Brady Bunch\" aired, ending a 5 year run." )
        , ( Tv, 1974, "The finale of \"Monty Python's Flying Circus\" aired, ending a 5 year run." )
        , ( Tv, 1977, "The finale of \"The Mary Tyler Moore Show\" aired, ending a 7 year run." )
        , ( Tv, 1978, "The finale of \"The Bob Newhart Show\" aired, ending a 6 year run." )
        , ( Tv, 1979, "The finale of \"All in the Family\" aired, ending a 8 year run." )
        , ( Tv, 1980, "The finale of \"Rockford Files\" aired, ending a 6 year run." )
        , ( Tv, 1982, "The finale of \"Mork & Mindy\" aired, ending a 4 year run." )
        , ( Tv, 1983, "The finale of \"Taxi\" aired, ending a 5 year run." )
        , ( Tv, 1983, "The finale of \"M*A*S*H\" aired, ending a 11 year run." )
        , ( Tv, 1987, "The finale of \"Hill Street Blues\" aired, ending a 6 year run." )
        , ( Tv, 1989, "The finale of \"Family Ties\" aired, ending a 7 year run." )
        , ( Tv, 1989, "The finale of \"Moonlighting\" aired, ending a 4 year run." )
        , ( Tv, 1991, "The finale of \"Twin Peaks\" aired, ending a 1 year run." )
        , ( Tv, 1992, "The finale of \"The Golden Girls\" aired, ending a 7 year run." )
        , ( Tv, 1993, "The finale of \"Saved by the Bell\" aired, ending a 14 year run." )
        , ( Tv, 1993, "The finale of \"The Wonder Years\" aired, ending a 5 year run." )
        , ( Tv, 1993, "The finale of \"Cheers\" aired, ending a 11 year run." )
        , ( Tv, 1994, "The finale of \"Star Trek: The Next Generation\" aired, ending a 7 year run." )
        , ( Tv, 1995, "The finale of \"Full House\" aired, ending a 8 year run." )
        , ( Tv, 1996, "The finale of \"The Fresh Prince of Bel-Air\" aired, ending a 6 year run." )
        , ( Tv, 1997, "The finale of \"Married.. With Children\" aired, ending a 10 year run." )
        , ( Tv, 1997, "The finale of \"Roseanne\" aired, ending a 9 year run." )
        , ( Tv, 1998, "The finale of \"Murphy Brown\" aired, ending a 10 year run." )
        , ( Tv, 1998, "The finale of \"Seinfeld\" aired, ending a 9 year run." )
        , ( Tv, 2002, "The finale of \"Ally McBeal\" aired, ending a 5 year run." )
        , ( Tv, 2002, "The finale of \"The X-Files\" aired, ending a 9 year run." )
        , ( Tv, 2003, "The finale of \"Firefly\" aired, ending a 1 year run." )
        , ( Tv, 2003, "The finale of \"Buffy the Vampire Slayer\" aired, ending a 6 year run." )
        , ( Tv, 2004, "The finale of \"Frasier\" aired, ending a 11 year run." )
        , ( Tv, 2004, "The finale of \"Sex and the City\" aired, ending a 6 year run." )
        , ( Tv, 2004, "The finale of \"Friends\" aired, ending a 10 year run." )
        , ( Tv, 2005, "The finale of \"Everybody Loves Raymond\" aired, ending a 9 year run." )
        , ( Tv, 2005, "The finale of \"Six Feet Under\" aired, ending a 4 year run." )
        , ( Tv, 2006, "The finale of \"The West Wing\" aired, ending a 7 year run." )
        , ( Tv, 2007, "The finale of \"Gilmore Girls\" aired, ending a 7 year run." )
        , ( Tv, 2007, "The finale of \"The Sopranos\" aired, ending a 8 year run." )
        , ( Tv, 2009, "The finale of \"Battlestar Galactica\" aired, ending a 5 year run." )
        , ( Tv, 2009, "The finale of \"ER\" aired, ending a 15 year run." )
        , ( Tv, 2010, "The finale of \"Scrubs\" aired, ending a 9 year run." )
        , ( Tv, 2010, "The finale of \"Law and Order\" aired, ending a 20 year run." )
        , ( Tv, 2010, "The finale of \"Lost\" aired, ending a 6 year run." )
        , ( Tv, 2011, "The finale of \"Entourage\" aired, ending a 7 year run." )
        , ( Tv, 2012, "The finale of \"House\" aired, ending a 8 year run." )
        , ( Tv, 2013, "The finale of \"Dexter\" aired, ending a 7 year run." )
        , ( Tv, 2013, "The finale of \"The Office\" aired, ending a 8 year run." )
        , ( Tv, 2013, "The finale of \"30 Rock\" aired, ending a 7 year run." )
        , ( Tv, 2013, "The finale of \"Breaking Bad\" aired, ending a 5 year run." )
        , ( Tv, 2014, "The finale of \"True Blood\" aired, ending a 6 year run." )
        , ( Tv, 2014, "The finale of \"How I Met Your Mother\" aired, ending a 9 year run." )
        , ( Tv, 2015, "The finale of \"Parks and Recreation\" aired, ending a 6 year run." )
        , ( Tv, 2015, "The finale of \"Mad Men\" aired, ending a 8 year run." )
        , ( Tv, 1955, "\"Peter Pan\" is broadcast on television for the first time" )
        , ( Politics, 1955, "The Warsaw Pact signed by the USSR and its allies." )
        , ( Tv, 1955, "\"The $64,000 Question\" premieres on CBS-TV " )
        , ( Movie, 1955, "\"Lady and the Tramp\", the Walt Disney company's 15th animated film,   premieres in Chicago." )
        , ( Tv, 1955, "The Mickey Mouse Club  debuts on the ABC-TV." )
        , ( Politics, 1955, "Rosa Parks  refuses give up her seat to make room for a white passenger and is arrested." )
        , ( Stage, 1949, "\"South Pacific\" won the Tony Award for best Musical." )
        , ( Stage, 1950, "\"Guys and Dolls\" won the Tony Award for best Musical." )
        , ( Stage, 1951, "\"The King and I\" won the Tony Award for best Musical." )
        , ( Stage, 1955, "\"Damn Yankees\" won the Tony Award for best Musical." )
        , ( Stage, 1956, "\"My Fair Lady\" won the Tony Award for best Musical." )
        , ( Stage, 1957, "\"The Music Man\" won the Tony Award for best Musical." )
        , ( Stage, 1959, "\"The Sound of Music\" won the Tony Award for best Musical." )
        , ( Stage, 1964, "\"Fiddler on the Roof\" won the Tony Award for best Musical." )
        , ( Stage, 1964, "\"Hello, Dolly!\" won the Tony Award for best Musical." )
        , ( Stage, 1965, "\"Man of La Mancha\" won the Tony Award for best Musical." )
        , ( Stage, 1966, "\"Cabaret\" won the Tony Award for best Musical." )
        , ( Stage, 1975, "\"A Chorus Line\" won the Tony Award for best Musical." )
        , ( Stage, 1975, "\"The Wiz\" won the Tony Award for best Musical." )
        , ( Stage, 1977, "\"Annie\" won the Tony Award for best Musical." )
        , ( Stage, 1978, "\"Evita\" won the Tony Award for best Musical." )
        , ( Stage, 1979, "\"Sweeney Todd: The Demon Barber of Fleet Street\" won the Tony Award for best Musical." )
        , ( Stage, 1980, "\"Les Misérables\" won the Tony Award for best Musical." )
        , ( Stage, 1981, "\"Cats\" won the Tony Award for best Musical." )
        , ( Stage, 1983, "\"La Cage aux Folles\" won the Tony Award for best Musical." )
        , ( Stage, 1986, "\"The Phantom of the Opera\" won the Tony Award for best Musical." )
        , ( Stage, 1996, "\"Rent\" won the Tony Award for best Musical." )
        , ( Stage, 1997, "\"The Lion King\" won the Tony Award for best Musical." )
        , ( Stage, 2002, "\"Hairspray\" won the Tony Award for best Musical." )
        , ( Stage, 2011, "\"The Book of Mormon\" won the Tony Award for best Musical." )
        , ( Stage, 2015, "\"Hamilton\" won the Tony Award for best Musical." )
        ]
