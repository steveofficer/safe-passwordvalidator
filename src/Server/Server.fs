module Server
open System
open System.Reflection
open Fable.Remoting.Server
open Fable.Remoting.Giraffe
open Saturn

open Shared

let api =
    {
      getInfo = fun () -> async { return { Version = Assembly.GetCallingAssembly().ImageRuntimeVersion; Time = DateTimeOffset.UtcNow } }
      save =
        fun password -> async {
            match Validator.validate password with
            | ValidPassword _ ->
                return Ok ()

            | InvalidPassword (password, policies) ->
                let message = sprintf "%s failed %d policies" password policies.Length
                return Error message
        }
    }

let webApp =
    Remoting.createApi()
    |> Remoting.withRouteBuilder Route.builder
    |> Remoting.fromValue api
    |> Remoting.buildHttpHandler

let app =
    application {
        url "http://0.0.0.0:8085"
        use_router webApp
        memory_cache
        use_static "public"
        use_gzip
    }

run app
