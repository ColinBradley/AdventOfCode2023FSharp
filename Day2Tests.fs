namespace AdventOfCode2023

open Microsoft.VisualStudio.TestTools.UnitTesting

type Toss = {
    Count: int
    Colour: string
}

type Round = {
    Tosses: Toss array
    TossesByColour: Map<string, int>
}

type Game = {
    Id: int
    Rounds: Round array
}

type ValidatedGame =
    | Valid of Game
    | Invalid of Game

[<TestClass>]
type Day2Tests () =

    let example1Lines = System.IO.File.ReadAllLines("./Data/Day2Example1.txt")
    let dayLines = System.IO.File.ReadAllLines("./Data/Day2.txt")

    let limitsByColourName = Map [|
        ("red", 12)
        ("green", 13)
        ("blue", 14)
    |]

    let ValidateGame (game: Game) =
        let isValid = 
            game.Rounds
            |> Array.forall (fun round -> 
                round.Tosses 
                |> Array.forall (fun toss -> 
                    match limitsByColourName.TryFind(toss.Colour) with
                    | Some(limit) -> toss.Count <= limit
                    | _ -> true
                )
            )

        if isValid then 
            Valid(game)
        else 
            Invalid(game)

    let ParseRounds (games: string) =
        games.Split(";") 
        |> Array.map (fun game -> 
            let tosses = 
                game.Split(",") 
                |> Array.map _.Trim()
                |> Array.map (fun (toss: string) -> 
                    match toss.Split(" ") with
                    | [| (count: string); (colour: string) |] ->
                        {
                            Count = int count;
                            Colour = colour
                        }
                    | _ -> raise (new System.Exception("bad toss"))
                )
            { 
                Tosses = tosses
                TossesByColour = 
                    tosses 
                    |> Array.groupBy _.Colour 
                    |> Array.map (fun (colour, tosses) -> (colour, tosses |> Array.sumBy _.Count))
                    |> Map
            }
        )

    let ParseGame (line: string) =
        match line.Split(":") with
            | [| game; rest |] -> 
                { 
                    Id = game.Substring(5) |> int; 
                    Rounds = ParseRounds rest 
                }
            | _ -> raise (new System.Exception("bad game"))

    let GetMaxColourCountFromRounds game colour =
        game.Rounds
        |> Array.map (fun round -> round.TossesByColour.TryFind(colour) |> Option.defaultValue 0)
        |> Array.max

    let SolvePart1 (lines: string array) = 
        lines 
        |> Array.map ParseGame
        |> Array.map ValidateGame
        |> Array.choose (fun game -> match game with | Valid(game) -> Some(game) | _ -> None)
        |> Array.sumBy _.Id

    let SolvePart2 (lines: string array) = 
        lines 
        |> Array.map ParseGame
        |> Array.map (fun game -> 
            let maxRed = GetMaxColourCountFromRounds game "red"
            let maxGreen = GetMaxColourCountFromRounds game "green"
            let maxBlue = GetMaxColourCountFromRounds game "blue"
            maxRed * maxGreen * maxBlue
        )
        |> Array.sum

    [<TestMethod>]
    member this.Part1Example () =
        Assert.AreEqual(
            8,
            SolvePart1 example1Lines
        )

    [<TestMethod>]
    member this.Part1 () =
        Assert.AreEqual(
            2169,
            SolvePart1 dayLines
        )

    [<TestMethod>]
    member this.Part2Example () =
        Assert.AreEqual(
            2286,
            SolvePart2 example1Lines
        )

    [<TestMethod>]
    member this.Part2 () =
        Assert.AreEqual(
            60948,
            SolvePart2 dayLines
        )