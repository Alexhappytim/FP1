module CollatzSolutions =

    open System
    open System.Collections.Generic

    // хвостовая
    module TailRecursive =
        let rec collatzLengthAcc (n: int64) (acc: int) : int =
            if n = 1L then acc
            elif n % 2L = 0L then collatzLengthAcc (n / 2L) (acc + 1)
            else collatzLengthAcc (3L * n + 1L) (acc + 1)

        let collatzLength n = collatzLengthAcc n 1

        let solve () =
            let rec findMax currentNum maxNum maxLength =
                if currentNum >= 1000000 then
                    (maxNum, maxLength)
                else
                    let length = collatzLength (int64 currentNum)

                    if length > maxLength then
                        findMax (currentNum + 1) currentNum length
                    else
                        findMax (currentNum + 1) maxNum maxLength

            findMax 1 1 1

    // рекурсия
    module NonTailRecursive =
        let rec collatzLength (n: int64) : int =
            if n = 1L then 1
            elif n % 2L = 0L then 1 + collatzLength (n / 2L)
            else 1 + collatzLength (3L * n + 1L)

        let solve () =
            let rec findMax currentNum maxNum maxLength =
                if currentNum >= 1000000 then
                    (maxNum, maxLength)
                else
                    let length = collatzLength (int64 currentNum)

                    if length > maxLength then
                        findMax (currentNum + 1) currentNum length
                    else
                        findMax (currentNum + 1) maxNum maxLength

            findMax 1 1 1

    // генераторы и свертки
    module Modular =
        let generateCollatzSeq (n: int64) =
            let rec generate current =
                seq {
                    yield current

                    if current <> 1L then
                        if current % 2L = 0L then
                            yield! generate (current / 2L)
                        else
                            yield! generate (3L * current + 1L)
                }

            generate n

        let countLength seq =
            seq |> Seq.fold (fun acc _ -> acc + 1) 0

        let collatzLength n = n |> generateCollatzSeq |> countLength

        let solve () =
            [ 1..999999 ]
            |> Seq.map (fun n -> (n, collatzLength (int64 n)))
            |> Seq.maxBy snd

    // Маппинг
    module MapBased =
        let generateCollatzList (n: int64) =
            let rec generate current acc =
                if current = 1L then
                    1L :: acc |> List.rev
                elif current % 2L = 0L then
                    generate (current / 2L) (current :: acc)
                else
                    generate (3L * current + 1L) (current :: acc)

            generate n []

        let solve () =
            [ 1..999999 ]
            |> List.map (fun n -> (n, generateCollatzList (int64 n) |> List.length))
            |> List.maxBy snd

    // for в функциональном стиле
    module LoopSyntax =
        let collatzLength (n: int64) =
            let mutable current = n
            let mutable length = 0

            while current <> 1L do
                length <- length + 1

                current <-
                    if current % 2L = 0L then
                        current / 2L
                    else
                        3L * current + 1L

            length + 1

        let solve () =
            seq {
                for i in 1..999999 do
                    yield (i, collatzLength (int64 i))
            }
            |> Seq.maxBy snd

    // ленивые вычисления и бесконечные списки
    module LazyInfinite =
        let collatzSeqUnfold (n: int64) =
            n
            |> Seq.unfold (fun state ->
                if state = 1L then Some(state, 0L)
                elif state = 0L then None
                elif state % 2L = 0L then Some(state, state / 2L)
                else Some(state, 3L * state + 1L))

        let collatzLength n = collatzSeqUnfold n |> Seq.length

        let naturalNumbers = Seq.initInfinite (fun i -> i + 1)

        let solve () =
            naturalNumbers
            |> Seq.takeWhile (fun n -> n < 1000000)
            |> Seq.map (fun n -> (n, collatzLength (int64 n)))
            |> Seq.maxBy snd
