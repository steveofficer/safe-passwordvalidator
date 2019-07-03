module Promises 

open Shared
open Fable.Core.JsInterop
open Fetch.Types

// This function is a promise that sends the password to the Web API
let savePassword (password: string) = promise {
    let! _ = 
        Fetch.fetch 
            "/api/save" 
            [ 
                Method(HttpMethod.POST) 
                Body(!^(sprintf "\"%s\"" password)) 
            ] 
    return ()
}

// This function is a promise that retrieves some arbitrary information from the Web API
let getServerInfo() = Thoth.Fetch.Fetch.fetchAs<ServerInfo> "/api/info" 