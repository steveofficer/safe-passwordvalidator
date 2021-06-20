namespace Shared

open System

type ServerInfo = {
    Version: string
    Time: DateTimeOffset
}

module Route =
    let builder typeName methodName =
        sprintf "/api/%s/%s" typeName methodName

type IApi =
    {
      getInfo : unit -> Async<ServerInfo>
      save : string -> Async<Result<unit, string>>
    }

