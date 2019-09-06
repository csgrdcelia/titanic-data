module Exercise

(*
If you want to execute part of this file in the REPL, you must first load the following files:
#load "Blank.fs"
#load "CsvParser.fs"
#load "LazyList.fs"
#load "LazyListModule.fs"
*)

open CsvParser
open HomeMadeCollections

// That's not actually what we want
// The dataset is described here:
// https://www.kaggle.com/c/titanic/data
type Gender = | Male | Female
type Passenger = {
    Name: string
    Age: Option<decimal>
    Gender: Gender
    Fare: decimal
    Survived: bool
}

let parseDecimal (s: string) = 
    System.Decimal.Parse(s, System.Globalization.CultureInfo.InvariantCulture)

let skipFirst list = 
    match list with 
    | [] -> invalidOp "The list is empty"
    | _ :: elements -> elements

let Run () =
    let file = System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__ + "/../CsvFiles/titanic.csv")
    let data =
        [|
            for line in skipFirst (Array.toList(file)) do
            yield parseLineWithRegex line
        |]

    // We want to map a line to a Passenger type
    let mapPassenger (a: string array) : Passenger = 
        {
            Name = a.[2]
            Age = if a.[4] = "" then None else Some(parseDecimal a.[4])
            Gender = if a.[3] = "male" then Male else Female
            Fare = System.Decimal.Parse(a.[8])
            Survived = a.[0] = "1"
        }
    
    mapPassenger (data.[4])

    // We want to load the list of all the passengers
    let passengers: Passenger list = [
        for line in data do 
        yield mapPassenger line 
    ]

    // Now we can start to answer questions!

    // How many passengers are there in this dataset?
    let passengersCount : int = passengers.Length

    // Who is the 247th passenger in this dataset?
    let passenger247 : Passenger = passengers.[246]

    // How many childen below 10 years old are there in this dataset?
    let passengersBelow10YearsOldCount : int = __

    // How many of them survived?
    let passengersBelow10YearsOldCountWhoSurvived : int = __

    // What is the most expensive fare paid to onboard?
    // Who paid the most expensive fare to onboard?
    // How many of them died/survived?
    let mostExpensiveFare : float = __
    let passengersWhoPaidTheMost : LazyList<Passenger> = __
    let passengersWhoPaidTheMostAndDiedCount : int = __
    let passengersWhoPaidTheMostAndSurvivedCount : int = __

    // What is the least expensive fare paid to onboard?
    // Who paid the least expensive fare to onboard?
    // How many of them survived?
    let leastExpensiveFare : float = __
    let passengersWhoPaidTheLeast : LazyList<Passenger> = __
    let passengersWhoPaidTheLeastAndDiedCount : int = __
    let passengersWhoPaidTheLeastAndSurvivedCount : int = __

    // Was there a passenger in cabin "42"?
    let wasThereAnyoneInCabin42 : bool = __

    // How many distinct cabins are there in the dataset?
    let distinctCabinsCount : int = __

    // Among the 100 first passengers in the file, what is the longest name?
    let longestNameAmongThe100FirstPassengers : string = __

    // What is the average age of passengers?
    // and standard deviation?
    let averageAge : float = __
    let standardDeviation : float = __

    // Can you compute both the average and the standard deviation in one pass?
    let (averageAge' : float, standardDeviation' : float) = __

    if averageAge' <> averageAge then
        failwith "averageAge' should equal averageAge"

    if standardDeviation' <> standardDeviation
        then failwith "standardDeviation' should equal standardDeviation"

    // What is the global survival rate?
    // What ist the survival rate for each passenger class / sex combination?
    let globalSurvivalRate : float = __

    let survivalRates : LazyList<(int * string) * float> = __

    printfn "All Good!"

