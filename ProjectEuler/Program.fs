open Problem1
open System
open System.Diagnostics
open System.Collections
open System.Collections.Generic
   
let timedOperation title operation =
    let stopwatch = new Stopwatch()
    stopwatch.Start()
    let result = operation ()
    Console.WriteLine(sprintf "%s: Time: %fs" title stopwatch.Elapsed.TotalSeconds)
    (result, stopwatch.Elapsed)

let generateRandomSeq size =
    
    let rnd = new System.Random()

    Seq.initInfinite(fun x -> x)
    |> Seq.takeWhile(fun x -> x < size)
    |> Seq.map(fun x -> rnd.Next(100))

let printList (list : System.Collections.Generic.List<'a>) =
    System.Console.WriteLine("[" + String.Join (" ; ", list) + "]")

let copyList (list : System.Collections.Generic.List<'a>) =
    new System.Collections.Generic.List<'a>(list)

let swap x y (list : System.Collections.Generic.List<'a>) =
    let temp = list.Item(x)
    list.Item(x) <- list.Item(y)
    list.Item(y) <- temp

let runOperationsOnRandomList sizes operations =
    sizes
    |> Seq.iter(fun size ->
        let randomList =
            generateRandomSeq size
            |> (fun x -> new System.Collections.Generic.List<int>(x))

        operations
        |> Seq.iter(fun (title, operation) ->
            timedOperation (sprintf "%s %i Items" title size) (operation (copyList randomList))
            |> ignore
        )
    )
    
let bubblesort (list : System.Collections.Generic.List<'a>) () =
    for x in [list.Count - 1 .. -1 .. 0] do
        for i in [0 .. x - 1 ] do
            for j in [i .. x] do
                if list.Item(i) > list.Item(j) then
                    swap i j list

let quicksort (list : System.Collections.Generic.List<int>) () =
    let rnd = new System.Random()

    let rec quicksortInner left right =
        if left < right then
            let pivotIndex = rnd.Next(right - left) + left
            let pivotValue = list.Item(pivotIndex)
        
            swap pivotIndex right list

            let mutable storeIndex = left
        
            for i in [left .. right] do
                if list.Item(i) < pivotValue then
                    swap storeIndex i list
                    storeIndex <- storeIndex + 1

            swap storeIndex right list
            
            quicksortInner left (storeIndex - 1)
            quicksortInner (storeIndex + 1) right

    quicksortInner 0 (list.Count - 1)

runOperationsOnRandomList
    [10 ; 100 ; 1000 ]
    [
        ("Quick Sort", quicksort) ;
        ("Bubble Sort", bubblesort)
    ]

System.Console.WriteLine("Press Enter to contine...")
System.Console.ReadLine()
|> ignore
