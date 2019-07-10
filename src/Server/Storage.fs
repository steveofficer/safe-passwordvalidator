module Storage

open Shared
open System.Security.Cryptography
open System.Text

let hash (password: Password) = 
    match password with
    | ValidPassword password -> 
        SHA256Managed.Create().ComputeHash(Encoding.UTF8.GetBytes password)
        |> Ok
    
    | InvalidPassword (_, policies) -> 
        policies 
        |> List.where (fun r -> not r.Result) 
        |> List.length
        |> sprintf "Password is invalid, %d policies failed."
        |> Error

let savePassword hashedPassword = async {
    printfn "%A" hashedPassword
}
    
