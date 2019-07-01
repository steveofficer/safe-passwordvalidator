namespace Shared

open System

type ServerInfo = {
    Version: string
    Time: DateTimeOffset
}

type PasswordPolicy = {
    Name: string
    IsValid: string -> Result<unit, string>
}

type Password = 
    | ValidPassword of string
    | InvalidPassword of string * PolicyResult list
and PolicyResult = {
    Name: string
    Result: Result<unit, string>
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

    let ``has one of`` (set: char Set) (password: string) = 
        if password |> Seq.exists set.Contains
        then Ok()
        else Error (sprintf "Expected one of %A" set)

    let ``does not have one of`` (set: char Set) (password: string) = 
        match (``has one of`` set password) with
        | Ok () -> Error (sprintf "Cannot contain any of %A" set)
        | Error _ -> Ok()
    
    let ``run rule against`` (password: string) (rule: PasswordPolicy) = 
        { Name = rule.Name; Result = rule.IsValid password }
    
    let ``has any`` = List.exists
    let (>>?) p f = p |> List.exists f

    let ``failed policies`` = function 
        | { Name = _ ; Result = Ok() } -> false 
        | { Name = _ ; Result = Error _ } -> true 

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

    member this.Validate (password) =
        let policies = 
            rules 
            |> List.map (``run rule against`` password)

        //if policies >>? ``failed policies``
        if policies |> ``has any`` ``failed policies``
        then InvalidPassword (password, policies)
        else ValidPassword password