namespace Shared

open System

type ServerInfo = {
    Version: string
    Time: DateTimeOffset
}

type Policy = {
    Name: string
    IsValid: string -> bool
}

type Password = 
    | ValidPassword of string
    | InvalidPassword of string * PolicyResult list
and PolicyResult = {
    Name: string
    Result: bool
}

type Validator() =
    let ``minimum length of`` length (password: string) =
        password.Length >= length

    let ``maximum length of`` length (password: string) = 
        password.Length <= length

    let ``has one of`` (chars: char Set) (password: string) = 
        password |> Seq.exists chars.Contains

    let ``does not have one of`` (chars: char Set) (password: string) = 
        ``has one of`` chars password |> not
    
    let ``run against`` (password: string) (rules: Policy list) = 
        rules |> List.map (fun rule -> { Name = rule.Name; Result = rule.IsValid password })
    
    let ``failed policies`` = function 
        | { Name = _ ; Result = true } -> false 
        | { Name = _ ; Result = false } -> true 

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

    let (|FailuresFound|AllPassed|) results = 
        if results |> List.exists ``failed policies``
        then FailuresFound results
        else AllPassed

    let rules = [
        ``minimum length rule`` 5
        ``maximum length rule`` 12
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
    ]    

    member this.Validate (password: string) =
        match rules |> ``run against`` password with
        | FailuresFound results -> InvalidPassword (password, results)
        | AllPassed -> ValidPassword password
