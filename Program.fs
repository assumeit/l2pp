type Coach = {
    Nme: string
    Player: bool
}

type Stats = {
    Wins: int
    Loss: int
}

type Team = {
    Nme: string
    Coach: Coach
    Stats: Stats
}

let coach1 = { Nme = "Erik Spoelstra"; Player = false }
let coach2 = { Nme = "Steve Kerr"; Player = true }
let coach3 = { Nme = "Tyronn Lue"; Player = true }
let coach4 = { Nme = "Monty Williams"; Player = false }
let coach5 = { Nme = "Tom Thibodeau"; Player = false }

let stats1 = { Wins = 44; Loss = 38 }
let stats2 = { Wins = 53; Loss = 29 }
let stats3 = { Wins = 44; Loss = 38 }
let stats4 = { Wins = 51; Loss = 31 }
let stats5 = { Wins = 47; Loss = 35 }

let team1 = { Nme = "Miami Heat"; Coach = coach1; Stats = stats1 }
let team2 = { Nme = "Golden State Warriors"; Coach = coach2; Stats = stats2 }
let team3 = { Nme = "Los Angeles Clippers"; Coach = coach3; Stats = stats3 }
let team4 = { Nme = "Phoenix Suns"; Coach = coach4; Stats = stats4 }
let team5 = { Nme = "New York Knicks"; Coach = coach5; Stats = stats5 }

let teams = [team1; team2; team3; team4; team5]

teams |> List.iter (fun team -> 
    printfn "Team: %s, Coach: %s, Wins: %d, Loss: %d" 
        team.Nme team.Coach.Nme team.Stats.Wins team.Stats.Loss)

let isSuccessful team =
    team.Stats.Wins > team.Stats.Loss

let successfulTeams = teams |> List.filter isSuccessful

successfulTeams |> List.iter (fun team ->
    printfn "Successful Team: %s, Coach: %s, Wins: %d, Loss: %d" 
        team.Nme team.Coach.Nme team.Stats.Wins team.Stats.Loss)

let successPercentage team =
    float team.Stats.Wins / float (team.Stats.Wins + team.Stats.Loss) * 100.0

let successPercentages = teams |> List.map successPercentage

successPercentages |> List.iter (fun sp -> 
    printfn "Success Percentage: %.2f%%" sp)

type Cuisine =
    | Korean
    | Turkish

type MovieType =
    | Regular
    | IMAX
    | DBOX
    | RegularWithSnacks
    | IMAXWithSnacks
    | DBOXWithSnacks

type Activity =
    | BoardGame
    | Chill
    | Movie of MovieType
    | Restaurant of Cuisine
    | LongDrive of int * float

let calculateBudget activity =
    match activity with
    | BoardGame -> 0.0
    | Chill -> 0.0
    | Movie genre ->
        match genre with
        | Regular -> 12.0 * 2.0
        | IMAX -> 17.0 * 2.0
        | DBOX -> 20.0 * 2.0
        | RegularWithSnacks -> (12.0 + 5.0) * 2.0
        | IMAXWithSnacks -> (17.0 + 5.0) * 2.0
        | DBOXWithSnacks -> (20.0 + 5.0) * 2.0
    | Restaurant cuisine ->
        match cuisine with
        | Korean -> 70.0
        | Turkish -> 65.0
    | LongDrive (kilometers, fuelCostPerKm) -> float kilometers * fuelCostPerKm

let activities = [
    BoardGame
    Chill
    Movie Regular
    Movie IMAXWithSnacks
    Restaurant Korean
    LongDrive (100, 0.15)
]

let budgets = activities |> List.map (fun activity ->
    let budget = calculateBudget activity
    printfn "Budget for %A: %.2f" activity budget
    budget
)

let totalBudget = List.sum budgets

printfn "Total Budget: %.2f" totalBudget
