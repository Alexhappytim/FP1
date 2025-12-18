#load "pow2 1000.fsx"
#load "3n+1.fsx"

open System
open System.Numerics
open Pow2Solutions
open CollatzSolutions

module TestFramework =
    type TestResult =
        | Success of string
        | Failure of string * string

    let mutable private testsPassed = 0
    let mutable private testsFailed = 0
    let mutable private currentSuite = ""

    let startSuite name =
        currentSuite <- name
        printfn ""
        printfn "=========================================="
        printfn "Test Suite: %s" name
        printfn "=========================================="

    let test name (assertion: unit -> bool) (expected: string) (actual: string) =
        try
            if assertion () then
                testsPassed <- testsPassed + 1
                printfn "✓ PASS: %s" name
                Success name
            else
                testsFailed <- testsFailed + 1
                printfn "✗ FAIL: %s" name
                printfn "  Expected: %s" expected
                printfn "  Actual:   %s" actual
                Failure(expected, actual)
        with ex ->
            testsFailed <- testsFailed + 1
            printfn "✗ ERROR: %s" name
            printfn "  Exception: %s" ex.Message
            Failure(expected, ex.Message)

    let assertEqual (expected: 'a) (actual: 'a) name =
        test name (fun () -> expected = actual) (sprintf "%A" expected) (sprintf "%A" actual)

    let assertTrue condition name =
        test name (fun () -> condition) "true" (sprintf "%b" condition)

    let assertNotNull value name =
        test name (fun () -> not (isNull (box value))) "not null" (if isNull (box value) then "null" else "not null")

    let summary () =
        printfn ""
        printfn "=========================================="
        printfn "Test Summary"
        printfn "=========================================="
        printfn "Total tests: %d" (testsPassed + testsFailed)
        printfn "Passed: %d" testsPassed
        printfn "Failed: %d" testsFailed
        printfn "Success rate: %.2f%%" (100.0 * float testsPassed / float (testsPassed + testsFailed))
        printfn "=========================================="

        if testsFailed > 0 then
            printfn ""
            printfn "::error::Tests failed! %d test(s) did not pass." testsFailed
            exit 1
        else
            printfn ""
            printfn "::notice::All tests passed! ✓"
            exit 0

module Power2Tests =
    open TestFramework

    // Хвостовая рекурсия
    let rec sumDigitsAcc (n: bigint) (acc: int) =
        if n = bigint.Zero then
            acc
        else
            sumDigitsAcc (n / bigint 10) (acc + int (n % bigint 10))

    let sumDigits n = sumDigitsAcc n 0

    // Обычная рекурсия
    let rec sumDigitsNonTail (n: bigint) =
        if n = bigint.Zero then
            0
        else
            int (n % bigint 10) + sumDigitsNonTail (n / bigint 10)

    // Маппинг
    let sumDigitsMap (n: bigint) =
        n.ToString() |> Seq.map (fun c -> int c - int '0') |> Seq.sum

    let runTests () =
        startSuite "Power of 2 - Sum of Digits Tests"

        // Тест на известном примере: 2^15 = 32768, сумма = 26
        let test15 = bigint.Pow(bigint 2, 15)
        assertEqual 32768I test15 "2^15 equals 32768" |> ignore

        assertEqual 26 (sumDigits test15) "Sum of digits of 2^15 (tail recursive)"
        |> ignore

        assertEqual 26 (sumDigitsNonTail test15) "Sum of digits of 2^15 (non-tail recursive)"
        |> ignore

        assertEqual 26 (sumDigitsMap test15) "Sum of digits of 2^15 (map-based)"
        |> ignore

        // Тест на малых степенях
        assertEqual 1 (sumDigits (bigint.Pow(bigint 2, 0))) "Sum of digits of 2^0 = 1"
        |> ignore

        assertEqual 2 (sumDigits (bigint.Pow(bigint 2, 1))) "Sum of digits of 2^1 = 2"
        |> ignore

        assertEqual 4 (sumDigits (bigint.Pow(bigint 2, 2))) "Sum of digits of 2^2 = 4"
        |> ignore

        assertEqual 8 (sumDigits (bigint.Pow(bigint 2, 3))) "Sum of digits of 2^3 = 8"
        |> ignore

        assertEqual 7 (sumDigits (bigint.Pow(bigint 2, 4))) "Sum of digits of 2^4 = 16 -> 7"
        |> ignore

        // Основной тест: 2^1000
        let result1000 = sumDigits (bigint.Pow(bigint 2, 1000))
        assertEqual 1366 result1000 "Sum of digits of 2^1000" |> ignore

        // Проверка конкретных методов из решения (6 способов)
        let expected = 1366

        assertEqual expected (Pow2Solutions.TailRecursive.solve ()) "Pow2 TailRecursive.solve = 1366"
        |> ignore

        assertEqual expected (Pow2Solutions.NonTailRecursive.solve ()) "Pow2 NonTailRecursive.solve = 1366"
        |> ignore

        assertEqual expected (Pow2Solutions.Modular.solve ()) "Pow2 Modular.solve = 1366"
        |> ignore

        assertEqual expected (Pow2Solutions.MapBased.solve ()) "Pow2 MapBased.solve = 1366"
        |> ignore

        assertEqual expected (Pow2Solutions.LazyInfinite.solve ()) "Pow2 LazyInfinite.solve = 1366"
        |> ignore

        // Проверка согласованности разных методов
        let power1000 = bigint.Pow(bigint 2, 1000)
        let tailResult = sumDigits power1000
        let nonTailResult = sumDigitsNonTail power1000
        let mapResult = sumDigitsMap power1000

        assertEqual tailResult nonTailResult "Tail and non-tail methods match" |> ignore
        assertEqual tailResult mapResult "Tail and map methods match" |> ignore

        // Тесты на граничные случаи
        assertEqual 0 (sumDigits bigint.Zero) "Sum of digits of 0" |> ignore
        assertEqual 1 (sumDigits bigint.One) "Sum of digits of 1" |> ignore
        assertEqual 9 (sumDigits (bigint 9)) "Sum of digits of 9" |> ignore
        assertEqual 1 (sumDigits (bigint 10)) "Sum of digits of 10" |> ignore
        assertEqual 10 (sumDigits (bigint 19)) "Sum of digits of 19" |> ignore

// ============================================================================
// Тесты для задачи Collatz (collatz.fsx)
// ============================================================================

module CollatzTests =
    open TestFramework

    // Хвостовая рекурсия
    let rec collatzLengthAcc (n: int64) (acc: int) : int =
        if n = 1L then acc
        elif n % 2L = 0L then collatzLengthAcc (n / 2L) (acc + 1)
        else collatzLengthAcc (3L * n + 1L) (acc + 1)

    let collatzLength n = collatzLengthAcc n 1

    // Обычная рекурсия
    let rec collatzLengthNonTail (n: int64) : int =
        if n = 1L then 1
        elif n % 2L = 0L then 1 + collatzLengthNonTail (n / 2L)
        else 1 + collatzLengthNonTail (3L * n + 1L)

    // Генерация последовательности
    let collatzSeq (n: int64) =
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

    let runTests () =
        startSuite "Collatz Problem Tests"

        // Тест на известном примере: 13 -> длина 10
        assertEqual 10 (collatzLength 13L) "Collatz length of 13 (tail recursive)"
        |> ignore

        assertEqual 10 (collatzLengthNonTail 13L) "Collatz length of 13 (non-tail recursive)"
        |> ignore

        assertEqual 10 (collatzSeq 13L |> Seq.length) "Collatz length of 13 (sequence)"
        |> ignore

        // Проверка последовательности для 13
        let seq13 = collatzSeq 13L |> Seq.toList
        let expected13 = [ 13L; 40L; 20L; 10L; 5L; 16L; 8L; 4L; 2L; 1L ]
        assertEqual expected13 seq13 "Collatz sequence for 13" |> ignore

        // Тесты на малых числах
        assertEqual 1 (collatzLength 1L) "Collatz length of 1" |> ignore
        assertEqual 2 (collatzLength 2L) "Collatz length of 2" |> ignore
        assertEqual 8 (collatzLength 3L) "Collatz length of 3" |> ignore
        assertEqual 3 (collatzLength 4L) "Collatz length of 4" |> ignore
        assertEqual 6 (collatzLength 5L) "Collatz length of 5" |> ignore

        // Тест на чётных и нечётных числах
        assertEqual 4 (collatzLength 8L) "Collatz length of 8 (power of 2)" |> ignore
        assertEqual 20 (collatzLength 9L) "Collatz length of 9" |> ignore
        assertEqual 5 (collatzLength 16L) "Collatz length of 16 (power of 2)" |> ignore

        let testNumbers = [ 1L; 5L; 13L; 27L; 97L; 100L; 1000L ]

        for num in testNumbers do
            let tailResult = collatzLength num
            let nonTailResult = collatzLengthNonTail num
            let seqResult = collatzSeq num |> Seq.length

            assertEqual tailResult nonTailResult (sprintf "Methods match for %d (tail vs non-tail)" num)
            |> ignore

            assertEqual tailResult seqResult (sprintf "Methods match for %d (tail vs seq)" num)
            |> ignore

        assertEqual 525 (collatzLength 837799L) "Collatz length of 837799" |> ignore

        let expectedLen = 525
        let n = 837799L

        assertEqual
            expectedLen
            (CollatzSolutions.TailRecursive.collatzLength n)
            "Collatz TailRecursive.collatzLength = 525"
        |> ignore

        assertEqual
            expectedLen
            (CollatzSolutions.NonTailRecursive.collatzLength n)
            "Collatz NonTailRecursive.collatzLength = 525"
        |> ignore

        assertEqual expectedLen (CollatzSolutions.Modular.collatzLength n) "Collatz Modular.collatzLength = 525"
        |> ignore

        assertEqual
            expectedLen
            (CollatzSolutions.MapBased.generateCollatzList n |> List.length)
            "Collatz MapBased length = 525"
        |> ignore


        assertEqual
            expectedLen
            (CollatzSolutions.LazyInfinite.collatzLength n)
            "Collatz LazyInfinite.collatzLength = 525"
        |> ignore

printfn "Starting Test Execution..."
printfn "=========================================="
printfn ""

// Запуск тестов
Power2Tests.runTests ()
CollatzTests.runTests ()

// Вывод общей статистики
TestFramework.summary ()
