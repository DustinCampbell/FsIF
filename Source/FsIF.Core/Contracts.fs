namespace FsIF.Core

exception ContractFailureException of string

[<RequireQualifiedAccess>]
module Is =

    let inline private fail message =
        raise <| ContractFailureException(message)

    let inline private failf fmt =
        Printf.ksprintf fail fmt

    let inline True condition =
        if condition = false then
            fail "condition should be true."

    let inline False condition =
        if condition = true then
            fail "condition should be false."

    let inline Null obj =
        if obj <> null then
            fail "obj should null."

    let inline Some obj =
        match obj with
        | Some(_) -> ()
        | None -> fail "obj should be Some."

    let inline None obj =
        match obj with
        | Some(_) -> fail "obj should be None."
        | None -> ()

    let inline EqualTo expected value =
        if value <> expected then
            failf "value should be equal to %A." expected

    let inline LessThan high value =
        if value >= high then
            failf "value should be less than %A" high

    let inline AtMost high value =
        if value > high then
            failf "value should be at most %A." high

    let inline GreaterThan low value =
        if value <= low then
            failf "value should be greater than %A" low

    let inline AtLeast low value =
        if value < low then
            failf "value should be at least %A" low

    let inline InRange low high value =
        if value < low || value > high then
            failf "value should be in range %A to %A." low high

    let inline Empty value =
        if not (value |> Seq.isEmpty) then
            fail "value should be empty."

    let inline OfType<'a> (value: obj) =
        match value with
        | :? 'a -> ()
        |     _ -> failf "value should be of type %s." typeof<'a>.FullName

    module Not =

        let inline Null obj =
            if obj = null then
                fail "obj should not be null."

        let inline EqualTo expected value =
            if value = expected then
                failf "value should be equal to %A." expected

        let inline InRange low high value =
            if value >= low && value <= high then
                failf "value should not be in range %A to %A." low high

        let inline Empty value =
            if value |> Seq.isEmpty then
                fail "value should be empty."

        let inline OfType<'a> (value: obj) =
            match value with
            | :? 'a -> failf "value should not be of type %s." typeof<'a>.FullName
            |     _ -> ()

