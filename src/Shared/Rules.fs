namespace Shared

type ErrorMessage = string

type PasswordPolicy = {
    Name: string
    IsValid: string -> Result<unit, ErrorMessage>
}

type Password =
    | ValidPassword of string
    | InvalidPassword of string * PolicyResult list
and PolicyResult = {
    Name: string
    Result: Result<unit, ErrorMessage>
}

module Rules =
    let ``minimum length of`` length (password: string) =
            if password.Length >= length
            then Ok()
            else Error (sprintf "%d is shorter than %d" password.Length length)

    let ``maximum length of`` length (password: string) =
        if password.Length <= length
        then Ok()
        else Error (sprintf "%d is longer than %d" password.Length length)

    let ``has one of`` (chars: char Set) (password: string) =
        if password |> Seq.exists chars.Contains
        then Ok()
        else Error (sprintf "Expected one of %A" chars)

    let ``does not have one of`` (chars: char Set) (password: string) =
        match (``has one of`` chars password) with
        | Ok() -> Error (sprintf "Cannot contain any of %A" chars)
        | Error _ -> Ok()

    let ``is not equal to`` (banned: string Set) (password: string) =
        match (Set.contains password banned) with
        | true -> Error (sprintf "Cannot contain any of %A" banned)
        | false -> Ok()

    let ``no repeated characters`` length (password: string) =
        password
        |> Seq.windowed length
        |> Seq.exists (fun s -> (s |> Array.distinct).Length = 1)
        |> function | true -> Error (sprintf "Cannot contain repeated characters") | false -> Ok()

    let ``minimum length rule`` length =
        {
            Name = sprintf "Minimum Length of %d" length
            IsValid = ``minimum length of`` length
        }

    let ``maximum length rule`` length =
        {
            Name = sprintf "Maximum Length of %d" length
            IsValid = ``maximum length of`` length
        }

    let ``no repeated characters rule`` length =
        {
            Name = sprintf "The same character no more than %d times in a row" length
            IsValid = ``no repeated characters`` length
        }

    let rules = [
        ``minimum length rule`` 5
        ``maximum length rule`` 15
        {
            Name = "Has a special character"
            IsValid = ``has one of`` <| set [ '%'; '$'; '!'; '&'; '_' ]
        }
        {
            Name = "Has a number"
            IsValid = ``has one of`` <| set [ '0'..'9' ]
        }
        {
            Name = "Has an uppercase Alpha"
            IsValid = ``has one of`` <| set [ 'A'..'Z' ]
        }
        {
            Name = "Does not have a banned character"
            IsValid = ``does not have one of`` <| set [ '*'; '''; '@' ]
        }
        {
            Name = "Is not banned word"
            IsValid = ``is not equal to`` <| set [ "password"; "1234" ]
        }

        ``no repeated characters rule`` 3
    ]

module Validator =
    let ``failed policy`` = function
        | { Name = _ ; Result = Ok() } -> false
        | { Name = _ ; Result = Error _ } -> true

    let ``run against`` (password: string) (rules: PasswordPolicy list) =
        rules |> List.map (fun rule -> { Name = rule.Name; Result = rule.IsValid password })

    let (|FailuresFound|AllPassed|) results =
        if results |> List.exists ``failed policy``
        then FailuresFound results
        else AllPassed

    let validate (password: string) =
        match Rules.rules |> ``run against`` password with
        | FailuresFound results -> InvalidPassword (password, results)
        | AllPassed -> ValidPassword password
