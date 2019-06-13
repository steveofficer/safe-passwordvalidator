module Client

open Elmish
open Elmish.React
open Fable.Core.JsInterop
open Fable.React
open Fable.React.Props
open Fetch.Types
open Thoth.Fetch
open Fulma
open Thoth.Json

open Shared
open System.Net.Http.Headers

// The model holds data that you want to keep track of while the application is running
// in this case, we are keeping track of a counter
// we mark it as optional, because initially it will not be available from the client
// the initial value will be requested from server
type Notification = 
    | Error of string
    | Warning of string
    | Message of string
type Model = { Password: Password option; Notification: Notification option }

// The Msg type defines what events/actions can occur while the application is running
// the state of the application changes *only* in reaction to these events
type Msg =
| ChangePassword of string
| SavePassword
| ShowNotification of Notification
| HideNotification

let savePassword (password: string) = promise {
    let! response = 
        Fetch.fetch 
            "/api/save" 
            [ RequestProperties.Method(Fetch.Types.HttpMethod.POST); RequestProperties.Body(!^(sprintf "\"%s\"" password)) ] 
    
    if response.Ok
    then return! Promise.lift()
    else 
        let! errorMessage = response.text()
        return! Promise.reject errorMessage
}
    

// defines the initial state and initial command (= side-effect) of the application
let init () : Model * Cmd<Msg> =
    let initialModel = { Password = None; Notification = None }
    initialModel, Cmd.none

// The update function computes the next state of the application based on the current state and the incoming events/messages
// It can also run side-effects (encoded as commands) like calling the server via Http.
// these commands in turn, can dispatch messages to which the update function will react.
let update (``validation result``) (msg : Msg) (currentModel : Model) : Model * Cmd<Msg> =
    match msg with
    
    | ChangePassword newPassword ->
        match newPassword with
        | "" -> { currentModel with Password = None }, Cmd.none
        | password -> { currentModel with Password = ``validation result`` password |> Some }, Cmd.none
    
    | SavePassword ->
        match currentModel.Password with
        | None -> { currentModel with Notification = "No password has been entered" |> Warning |> Some }, Cmd.none
        | Some password ->
            match password with
            | ValidPassword password -> 
                let cmd = 
                    Cmd.OfPromise.either
                        savePassword password
                        (fun () -> "Saved" |> Message |> ShowNotification)
                        (fun err -> err.Message |> Error |> ShowNotification)
                currentModel, cmd
            | InvalidPassword (password, _) -> 
                //{ currentModel with Notification = "An invalid password cannot be saved" |> Warning |> Some }, Cmd.none
                let cmd = 
                    Cmd.OfPromise.either
                        savePassword password
                        (fun () -> "Saved" |> Message |> ShowNotification)
                        (fun err -> 
                            err.Message |> Error |> ShowNotification)
                currentModel, cmd

    | ShowNotification notification -> { currentModel with Notification = Some notification }, Cmd.none
    
    | HideNotification -> { currentModel with Notification = None }, Cmd.none

let view (model : Model) (dispatch : Msg -> unit) =
    let ``policy result`` (policy: PolicyResult) =
        let icon = 
            match policy with
            | { Name = _; IsSuccess = true } -> span [ Class "icon has-text-success"] [ i [ Class "fas fa-check-square" ] [] ]
            | { Name = _; IsSuccess = false } -> span [ Class "icon has-text-warning"] [ i [ Class "fas fa-exclamation-triangle" ] [] ]
        
        div [] [
            str policy.Name
            icon
        ]

    let ``show error`` dispatch = function
        | None -> null
        | Some notification ->
            let (t, message) = 
                match notification with
                | Error e -> "is-danger", e
                | Warning e -> "is-warning", e
                | Message m -> "is-success", m
            
            Notification.notification [ Notification.CustomClass t ] [
                Notification.delete [ GenericOption.Props [ OnClick (fun _ -> HideNotification |> dispatch) ]] []
                str message
            ]

    let ``show details`` password =
        Section.section [] [
            (match password with
            | None -> str "Nothing entered yet 👎"
            | Some password ->
                match password with
                | ValidPassword _ -> 
                    str "That's a valid password 👍"
                | InvalidPassword (_, policies) -> 
                    div [] (policies |> List.map ``policy result``))
        ]                
    
    Container.container [] [
        ``show error`` dispatch model.Notification

        Heading.h3 [] [ str "Password Validator" ]

        div [] [
            Field.div [] [ 
                Label.label [] [ str "Password" ]
                Control.div [] [
                    Input.password [ Input.OnChange (fun e -> ChangePassword e.Value |> dispatch) ]
                ]
            ]

            Button.button [ Button.Option.OnClick (fun _ -> SavePassword |> dispatch) ] [ str "Save password" ]
        ]

        ``show details`` model.Password
    ]

#if DEBUG
open Elmish.Debug
open Elmish.HMR
#endif

Program.mkProgram init (Validator().Validate |> update) view
#if DEBUG
|> Program.withConsoleTrace
#endif
|> Program.withReactBatched "elmish-app"
#if DEBUG
|> Program.withDebugger
#endif
|> Program.run
