open System
open System.IO

let courseTopicsFile =
    if fsi.CommandLineArgs.Length < 2 then
        printfn "Include course topics file name on the command line."
        Environment.Exit(1)
        String.Empty
    else
        fsi.CommandLineArgs.[1]

let courseTopics =
    // File.ReadAllLines("Geometry.txt")
    // File.ReadAllLines("ArtificialIntelligence.txt")
    courseTopicsFile
    |> File.ReadAllLines
    |> Array.where(String.IsNullOrEmpty >> not)
    |> Array.toList

// course calendar
let firstDay = DateTime(2019, 9, 12)
let lastDay = DateTime(2019, 12, 05)
let classMeetings = [ DayOfWeek.Thursday ] //; DayOfWeek.Wednesday ]
let daysOff = [
    (DateTime(2019, 09, 02), DateTime(2019, 09, 02), "Labor Day")
    (DateTime(2019, 09, 17), DateTime(2019, 09, 17), "Faith and Reason")
    (DateTime(2019, 10, 10), DateTime(2019, 10, 11), "October Break")
    (DateTime(2019, 11, 01), DateTime(2019, 11, 05), "Fall Break")
    (DateTime(2019, 11, 27), DateTime(2019, 11, 29), "Thanksgiving Break")
]

// let firstDay = DateTime(2019, 1, 23)
// let lastDay = DateTime(2019, 4, 29)
// // let classMeetings = [ DayOfWeek.Monday; DayOfWeek.Tuesday; DayOfWeek.Wednesday, DayOfWeek.Thursday ]
// // let classMeetings = [ DayOfWeek.Monday; DayOfWeek.Wednesday ]
// let classMeetings = [ DayOfWeek.Saturday ]

// let daysOff = [
//     (DateTime(2019, 01, 21), DateTime(2019, 01, 21), "MLK Jr. Day")
//     (DateTime(2019, 03, 04), DateTime(2019, 03, 08), "Spring Break")
//     (DateTime(2019, 04, 18), DateTime(2019, 04, 22), "Easter Break")
// ]

// functional utils
let dayOfWeekString (day: DateTime) = day.ToString("ddd")
let dayString (day: DateTime) = day.ToString("d-MMM-yyyy")
let isMeeting day = classMeetings |> Seq.contains day
let afterLastDay day = (day > lastDay)
let isDayOff day =
    daysOff
    |> Seq.tryFind(fun (d1, d2, _) -> d1 <= day && d2 >= day)
    |> Option.bind(fun (_, _, desc) -> Some desc)

// main loop
let rec nextDay (count: int) (topics: string list) (next: DateTime) =
    match next |> afterLastDay with
    | true -> topics |> List.iter(printfn "%s")
    | false -> // consider this day
        match next.DayOfWeek |> isMeeting with
        | false -> None, topics, String.Empty // not meeting, proceed to next day
        | true -> // meeting this day
            match next |> isDayOff with
            | Some(desc) -> None, topics, desc // holiday
            | None ->
                match topics with
                | [] -> [], String.Empty
                | [h] -> [], h
                | h::t -> t, h
                |> fun (tpcs, desc) -> Some(count + 1), tpcs, desc // regular work day
        |> fun(cnt, tpcs, desc) ->
            // display output
            if not <| String.IsNullOrWhiteSpace desc then
                let meetingCount = match cnt with Some(c) -> c.ToString() | None -> String.Empty
                let weekCount = next.Subtract(firstDay).Days / 7 + 1
                printfn "%2d %2s %3s %11s %s" weekCount meetingCount (dayOfWeekString next) (dayString next) desc
            // advance to next day
            nextDay (if cnt.IsSome then cnt.Value else count) tpcs (next.AddDays(1.0))

// start main loop 
nextDay 0 courseTopics firstDay