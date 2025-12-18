module Pow2Solutions

open System
open System.Numerics
open System.Runtime.CompilerServices

// хвостовая
module TailRecursive =
    [<TailCall>]
    let rec sumDigitsAcc (n: bigint) (acc: int) =
        if n = bigint.Zero then
            acc
        else
            let digit = int (n % bigint 10)
            sumDigitsAcc (n / bigint 10) (acc + digit)

    let sumDigits n = sumDigitsAcc n 0

    let solve () =
        let power = bigint.Pow(bigint 2, 1000)
        sumDigits power

// рекурсия
module NonTailRecursive =
    [<TailCall>]
    let rec sumDigits (n: bigint) =
        if n = bigint.Zero then
            0
        else
            let digit = int (n % bigint 10)
            digit + sumDigits (n / bigint 10)

    let solve () =
        let power = bigint.Pow(bigint 2, 1000)
        sumDigits power

// генераторы и свертки
module Modular =
    let generateDigits (n: bigint) =
        let rec generate num =
            seq {
                if num > bigint.Zero then
                    yield int (num % bigint 10)
                    yield! generate (num / bigint 10)
            }

        generate n

    let foldDigits digits = digits |> Seq.sum

    let solve () =
        let power = bigint.Pow(bigint 2, 1000)
        power |> generateDigits |> foldDigits

// Маппинг
module MapBased =
    let solve () =
        let power = bigint.Pow(bigint 2, 1000)
        let powerStr = power.ToString()
        powerStr |> Seq.sumBy (fun c -> int c - int '0')

// ленивые вычисления и бесконечные списки
module LazyInfinite =
    let powersOfTwo = Seq.initInfinite (fun i -> bigint.Pow(bigint 2, i))

    let digitsLazy (n: bigint) =
        n
        |> Seq.unfold (fun state ->
            if state = bigint.Zero then
                None
            else
                Some(int (state % bigint 10), state / bigint 10))

    let solve () =
        let power = powersOfTwo |> Seq.item 1000
        power |> digitsLazy |> Seq.sum
