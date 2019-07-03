open System.IO

open FSharp.Control.Tasks.V2
open Giraffe
open Saturn
open Shared
open System
open System.Reflection

let tryGetEnv = System.Environment.GetEnvironmentVariable >> function null | "" -> None | x -> Some x

let publicPath = Path.GetFullPath "../Client/public"

let port = "SERVER_PORT" |> tryGetEnv |> Option.map uint16 |> Option.defaultValue 8085us

let webApp = router {
    // Returns some arbitrary information back to the caller
    get "/api/info" (fun next ctx -> task {
        return! json { Version = Assembly.GetCallingAssembly().ImageRuntimeVersion; Time = DateTimeOffset.UtcNow } next ctx
    })
    
    // If the password is valid returns 200, otherwise it returns a 400 and an error message
    post "/api/save" (fun next ctx ->
        task {
            let! password = ctx.BindJsonAsync<string>()

            let hashedPassword = 
                password
                |> Validator().Validate 
                |> Storage.hash
            
            match hashedPassword with
            | Ok hash -> 
                do! Storage.savePassword hash
                return! Response.ok ctx "Password was saved"
            | Error message -> 
                return! Response.badRequest ctx message
        })
}

let app = application {
    url (sprintf "http://0.0.0.0:%d/" port)
    use_router webApp
    use_static publicPath
    use_json_serializer(Thoth.Json.Giraffe.ThothSerializer())
}

run app
