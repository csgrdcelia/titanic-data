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
type PassengerClass = | FirstClass | SecondClass | ThirdClass
type Passenger = {
    Name: string
    Age: Option<decimal>
    Gender: Gender
    Fare: decimal
    Survived: bool
    Cabin: Option<string>
    PassengerClass: PassengerClass
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
            Cabin = if a.[9] = "" then None else Some (a.[9])
            PassengerClass = 
                match a.[1] with
                | "1" -> FirstClass
                | "2" -> SecondClass
                | "3" -> ThirdClass
                | _ -> invalidArg "pclass" "Invalid passenger class"
        }
    
    mapPassenger (data.[4])

    // We want to load the list of all the passengers
    let passengers: Passenger list = [
        for line in data do 
        yield mapPassenger line 
    ]
    (* let passengers: Passenger list = 
        List.map mapPassenger data *)

    // Now we can start to answer questions!

    // How many passengers are there in this dataset?
    let passengersCount : int = passengers.Length

    (* 
        Who is the 247th passenger in this dataset?
    *)
    let rec getItemAtIndex index list =
        match list with
        | [] -> invalidArg "index" "Index out of bounds"
        | head :: tail ->
            if index = 0 then head 
            else getItemAtIndex (index - 1) tail

    //let passenger247 : Passenger = passengers.[246]
    let passenger247 : Passenger = getItemAtIndex 246 passengers

    (* 
        How many childen below 10 years old are there in this dataset?
    *)
    let rec getPassengersBelow10YearsOldCount (list: Passenger list) countSoFar = 
        match list with 
        | [] -> countSoFar
        | passenger :: otherPassengers ->
            match passenger.Age with
            | Some age when age <= 10M ->
                getPassengersBelow10YearsOldCount otherPassengers (countSoFar + 1)
            | _ -> 
                getPassengersBelow10YearsOldCount otherPassengers (countSoFar)

    //let passengersBelow10YearsOldCount : int = 
    //    getPassengersBelow10YearsOldCount passengers 0

    let passengersBelow10YearsOldCount : int = 
        passengers
        |> List.filter (fun p -> p.Age.IsSome && p.Age.Value <= 10M)
        |> List.length

    // OR 

    passengers
    |> List.choose (fun p -> p.Age) // enlève toutes les valeurs none
    |> List.filter (fun age -> age <= 10M)
    |> List.length

    (* 
        How many of them survived?
    *)
    let rec getPassengersBelow10YearsWhoSurvivedOldCount (list: Passenger list) countSoFar = 
        match list with 
        | [] -> countSoFar
        | passenger :: otherPassengers ->
            match passenger.Age with
            | Some age when age <= 10M && passenger.Survived ->
                getPassengersBelow10YearsWhoSurvivedOldCount otherPassengers (countSoFar + 1)
            | _ -> 
                getPassengersBelow10YearsWhoSurvivedOldCount otherPassengers (countSoFar)
    //let passengersBelow10YearsOldCountWhoSurvived : int = getPassengersBelow10YearsWhoSurvivedOldCount passengers 0
    let passengersBelow10YearsOldCountWhoSurvived : int = 
        passengers
        |> List.filter (fun s -> s.Age.IsSome && s.Age.Value <= 10M && s.Survived) 
        |> List.length 

    // What is the most expensive fare paid to onboard?
    // Who paid the most expensive fare to onboard?
    // How many of them died/survived?
    let mostExpensiveFare = 
        passengers
        |> List.map (fun p -> p.Fare)
        |> List.sortDescending
        |> List.head
    let passengersWhoPaidTheMost = 
        passengers
        |> List.filter (fun p -> p.Fare = mostExpensiveFare)

    let passengersWhoPaidTheMostAndDiedCount : int = 
        passengersWhoPaidTheMost
        |> List.filter (fun p -> not p.Survived)
        |> List.length
    
    let passengersWhoPaidTheMostAndSurvivedCount : int = 
        passengersWhoPaidTheMost
        |> List.filter (fun p -> p.Survived)
        |> List.length
        
    // What is the least expensive fare paid to onboard?
    // Who paid the least expensive fare to onboard?
    // How many of them survived?
    let leastExpensiveFare : float = __
    let passengersWhoPaidTheLeast : LazyList<Passenger> = __
    let passengersWhoPaidTheLeastAndDiedCount : int = __
    let passengersWhoPaidTheLeastAndSurvivedCount : int = __

    // Was there a passenger in cabin "B42"?
    let wasThereAnyoneInCabin42 : bool = 
        passengers
        |> List.exists (fun p -> p.Cabin = Some "B42")

    // How many distinct cabins are there in the dataset?
    let distinctCabinsCount : int = 
        passengers 
        |> List.choose (fun p -> p.Cabin)
        |> List.distinct
        |> List.length

    // Among the 100 first passengers in the file, what is the longest name?
    let longestNameAmongThe100FirstPassengers : string = 
        passengers
        |> List.take 100 
        |> List.map (fun p -> p.Name)
        |> List.maxBy (fun name -> name.Length)

    // What is the average age of passengers?
    // and standard deviation?

    let ages =
        passengers
        |> List.choose (fun p -> p.Age)
        |> List.map float

    let averageAge : float = 
        ages
        |> List.average

    let standardDeviation : float = 
        ages
        |> List.map (fun age -> pown (age - averageAge) 2)
        |> List.average
        |> sqrt
        
    // What is the global survival rate?
    let survivorsCount = 
        passengers
        |> List.filter (fun p -> p.Survived)
        |> List.length

    let globalSurvivalRate : float = 
        float survivorsCount / float passengersCount

    printfn "All Good!"

