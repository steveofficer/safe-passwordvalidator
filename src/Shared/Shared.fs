namespace Shared

type PasswordRule = {
    Name: string
    IsValid: string -> bool
}

type Password = 
    | ValidPassword of string
    | InvalidPassword of string * PolicyResult list
and PolicyResult = {
    Name: string
    IsSuccess: bool
}

type Validator() =
    let ``minimum length of`` length (password: string) = password.Length >= length
    let ``maximum length of`` length (password: string) = password.Length <= length
    let ``has one of`` (set: char Set) (password: string) = password |> Seq.exists set.Contains
    let ``does not have one of`` (set: char Set) = (``has one of`` set) >> not
    
    let ``run rule against`` (password: string) (rule: PasswordRule) = { Name = rule.Name; IsSuccess = rule.IsValid password }
    
    let ``has any`` = List.exists
    let (>>?) p f = p |> List.exists f

    let ``failed policies`` (rule: PolicyResult) = rule.IsSuccess |> not

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
            IsValid = [ '%'; '$'; '!'; '&'; '_' ] |> Set.ofList |> ``has one of`` 
        }
        {
            Name = "Has a number"
            IsValid = [ '0'..'9' ] |> Set.ofList |> ``has one of`` 
        }
        {
            Name = "Has an uppercase Alpha"
            IsValid = [ 'A'..'Z' ] |> Set.ofList |> ``has one of`` 
        }
        {
            Name = "Does not have banned character"
            IsValid = [ '*'; ''' ] |> Set.ofList |> ``does not have one of`` 
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