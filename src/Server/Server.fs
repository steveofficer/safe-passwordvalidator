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
    get "/api/info" (fun next ctx -> task {
        do! System.Threading.Tasks.Task.Delay(1000)
        return! json { Version = Assembly.GetCallingAssembly().ImageRuntimeVersion; Time = DateTimeOffset.UtcNow } next ctx
    })
    
    post "/api/save" (fun next ctx ->
        task {
            let! password = ctx.BindJsonAsync<string>()
            let validatorResult = Validator().Validate password
            match validatorResult with
            | ValidPassword password -> return! Response.ok ctx "Password was saved"
            | InvalidPassword (password, policies) -> return! Response.badRequest ctx "Password was invalid"
        })
}

let app = application {
    url (sprintf "http://0.0.0.0:%d/" port)
    use_router webApp
    use_static publicPath
    use_json_serializer(Thoth.Json.Giraffe.ThothSerializer())
}

run app
