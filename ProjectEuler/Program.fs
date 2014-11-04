﻿open System
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
    |> Seq.map(fun x -> rnd.Next(1000))

let printList title (list : System.Collections.Generic.List<'a>) =
    match String.IsNullOrWhiteSpace(title) with
    | true -> System.Console.WriteLine(" [" + String.Join (" ; ", list) + "]")
    | false -> System.Console.WriteLine(title + ": [" + String.Join (" ; ", list) + "]")
    

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
            let copiedList = (copyList randomList)

            if copiedList.Count <= 10 then printList "Unsorted" copiedList

            timedOperation (sprintf "%s %i Items" title size) (operation copiedList)
            |> ignore

            if copiedList.Count <= 10 then printList "Sorted" copiedList
        )

        System.Console.WriteLine(String.Empty)
    )
    
let bubblesort (list : System.Collections.Generic.List<'a>) () =
    for x in [list.Count - 1 .. -1 .. 0] do
        for i in [0 .. x - 1 ] do
            for j in [i .. x] do
                if list.Item(i) > list.Item(j) then
                    swap i j list

let insertionsort (list : System.Collections.Generic.List<'a>) () =
    for i in [1 .. list.Count - 1 ] do
        [i .. -1 .. 1]
        |> Seq.takeWhile(fun  j -> list.Item(j-1) > list.Item(j))
        |> Seq.iter(fun j -> swap (j-1) j list)

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

let mergesort (list : System.Collections.Generic.List<int>) () =

    let merge list1 list2 =
        Seq.unfold (fun (list1 : 'a list, list2 : 'a list) ->
            match list1.IsEmpty, list2.IsEmpty with
            | false, false -> match list1.Head, list2.Head with
                              | x , y when x <= y -> Some(list1.Head, (list1.Tail, list2))
                              | x , y -> Some(list2.Head, (list1, list2.Tail))
            | false, _ -> Some(list1.Head, (list1.Tail, list2))
            | _, false -> Some(list2.Head, (list1, list2.Tail))
            | _ -> None
        ) (list1, list2)
        |> Seq.toList

    let rec mergesortInner (list : 'a list) = 
        let length = list.Length
        if length < 2 then
            list
        else
            let middle = length / 2

            let left = 
                [0 .. (middle - 1)]
                |> List.map(fun i -> List.nth list i)

            let right = 
                [middle .. (length - 1)]
                |> List.map(fun i -> List.nth list i)

            (mergesortInner left, mergesortInner right)
            |> fun (sortedLeft, sortedRight) -> merge sortedLeft sortedRight

    let result =
        List.ofSeq list
        |> mergesortInner

    list.Clear()
    list.AddRange(result)


runOperationsOnRandomList [
        10 ;
        100 ;
        1000  ;
    ] [
        ("Merge Sort", mergesort) ;
        ("Insertion Sort", insertionsort) ;
        ("Quick Sort", quicksort) ;
        ("Bubble Sort", bubblesort) ;
    ]

System.Console.WriteLine("Press any key to contine...")
System.Console.Read()
|> ignore
