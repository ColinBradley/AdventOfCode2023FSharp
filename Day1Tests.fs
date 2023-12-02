namespace AdventOfCode2023

open System
open System.Resources
open System.Reflection
open Microsoft.VisualStudio.TestTools.UnitTesting

[<TestClass>]
type Day1Tests () =
    let resources = (new ResourceManager("AdventOfCode2023.Resources", Assembly.GetExecutingAssembly()))
    let day1Data = resources.GetString "Day1"

    let part1ExampleData = "1abc2
pqr3stu8vwx
a1b2c3d4e5f
treb7uchet"

    let part2ExampleData = "two1nine
eightwothree
abcone2threexyz
xtwone3four
4nineeightseven2
zoneight234
7pqrstsixteen"

    let part2ExmapleData2 = "twone"

    let numbers = [|
            ("one", "1");
            ("two", "2");
            ("three", "3");
            ("four", "4");
            ("five", "5");
            ("six", "6");
            ("seven", "7");
            ("eight", "8");
            ("nine", "9")
        |]

    let SolvePart1 (data: string) =
        data.Split("\r\n")
        |> Array.map (fun line -> 
            line.ToCharArray() 
            |> Array.filter Char.IsDigit
            |> (fun numberChars -> string (Array.head numberChars) + string (Array.last numberChars))
            |> int
        )
        |> Array.sum

    let GetNumbers (text: string) =
        let mutable index = 0
        seq {
            while index < text.Length do
                let firstChar = text.Chars(index)
                if Char.IsDigit firstChar then
                    yield string firstChar
                else
                    let rest = text.Substring(index)
                    match numbers 
                        |> Array.map (fun (word, value) -> if rest.StartsWith(word) then Some value else None)
                        |> Array.tryFind _.IsSome
                        |> Option.map _.Value with
                        | Some value -> 
                            yield value
                        | None -> ()
                index <- index + 1
        }

    let SolvePart2 (data: string) =
        data.Split("\r\n")
        |> Array.map (fun line -> 
            GetNumbers line
            |> fun numberCharsSeq -> 
                Seq.toArray numberCharsSeq 
                |> fun numberCharsArray -> 
                    Array.head numberCharsArray 
                    + Array.last numberCharsArray
            |> int
        )
        |> Array.sum

    [<TestMethod>]
    member this.Part1Example () =
        Assert.AreEqual(
            142,
            SolvePart1 part1ExampleData
        )

    [<TestMethod>]
    member this.Part1 () =
        Assert.AreEqual(
            54081,
            SolvePart1 day1Data
        )

    [<TestMethod>]
    member this.Part2Example () =
        Assert.AreEqual(
            281,
            SolvePart2 part2ExampleData
        )

    [<TestMethod>]
    member this.Part2Example2 () =
        Assert.AreEqual(
            21,
            SolvePart2 part2ExmapleData2
        )

    [<TestMethod>]
    member this.Part2 () =
        Assert.AreEqual(
            54649,
            SolvePart2 day1Data
        )