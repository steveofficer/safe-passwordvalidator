namespace Shared

open System

type ErrorMessage = string

type ServerInfo = {
    Version: string
    Time: DateTimeOffset
}

type Policy = {
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

type Validator() =
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
    
    let ``run against`` (password: string) (rules: Policy list) = 
        rules |> List.map (fun rule -> { Name = rule.Name; Result = rule.IsValid password })
    
    let ``failed policies`` = function 
        | { Name = _ ; Result = Ok() } -> false 
        | { Name = _ ; Result = Error _ } -> true 

    let (|FailuresFound|AllPassed|) results = 
        if results |> List.exists ``failed policies``
        then FailuresFound results
        else AllPassed

    let rules = [
        { 
            Name = "Minimum Length of 5"
            IsValid = ``minimum length of`` 5
        }
        { 
            Name = "Maximum Length of 12"
            IsValid = ``maximum length of`` 12
        }
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
