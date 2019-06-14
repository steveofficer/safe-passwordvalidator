module Client

open Elmish
open Elmish.React
open Fable.Core.JsInterop
open Fable.React
open Fetch.Types
open Fulma

open Shared
open Types
open ViewComponents

// This function is used to communicate the Web Server backend
// by using the browser's `fetch` api
let savePassword (password: string) = promise {
    let! response = 
        Fetch.fetch 
            "/api/save" 
            [ RequestProperties.Method(Fetch.Types.HttpMethod.POST); RequestProperties.Body(!^(sprintf "\"%s\"" password)) ] 
    return ()
}
    

// defines the initial state and initial command (a.k.a side-effect) of the application
let init () : Model * Cmd<Msg> =
    let initialModel = { Password = None; Notification = None }
    initialModel, Cmd.none

// The update function computes the next state of the application based on the current state and the incoming events/messages
// It can also run side-effects (encoded as commands) like calling the server via Http.
// These commands in turn, can dispatch messages to which the update function will react.
let update (msg : Msg) (currentModel : Model) : Model * Cmd<Msg> =
    match msg with
    
    | SetPassword newPassword ->
        match newPassword with
        | "" -> { currentModel with Password = None }, Cmd.none
        | password -> { currentModel with Password = password |> Validator().Validate |> Some }, Cmd.none
    
    | SavePassword ->
        match currentModel.Password with
        | None -> { currentModel with Notification = "No password has been entered" |> Warning |> Some }, Cmd.none
        | Some password ->
            match password with
            | ValidPassword password -> 
                let cmd = 
                    Cmd.OfPromise.either
                        savePassword password
                        (fun () -> "The server accepted the password" |> Message |> ShowNotification)
                        (fun _ -> "The server rejected the password" |> Error |> ShowNotification)
                currentModel, cmd
            | InvalidPassword (password, _) -> 
                { currentModel with Notification = "An invalid password cannot be saved" |> Warning |> Some }, Cmd.none
                // let cmd = 
                //     Cmd.OfPromise.either
                //         savePassword password
                //         (fun () -> "Saved" |> Message |> ShowNotification)
                //         (fun _ -> "The server rejected the password" |> Error |> ShowNotification)
                //currentModel, cmd

    | ShowNotification notification -> { currentModel with Notification = Some notification }, Cmd.none
    
    | HideNotification -> { currentModel with Notification = None }, Cmd.none

// This defines the visual elements that are rendered to the screen
// The elements that get displayed as well as the values are based on the applicatio's Model
let view (model : Model) (dispatch : Msg -> unit) =
    Container.container [] [
        ``notification message`` dispatch model.Notification

        Heading.h3 [] [ str "Password Validator" ]

        div [] [
            Field.div [] [ 
                Label.label [] [ str "Password" ]
                Control.div [] [
                    Input.password [ Input.OnChange (fun e -> SetPassword e.Value |> dispatch) ]
                ]
            ]

            Button.button [ Button.Option.OnClick (fun _ -> SavePassword |> dispatch) ] [ str "Save password" ]
        ]

        ``password status`` model.Password
    ]

// This start the application
#if DEBUG
open Elmish.Debug
open Elmish.HMR
#endif

Program.mkProgram init update view
#if DEBUG
|> Program.withConsoleTrace
#endif
|> Program.withReactBatched "elmish-app"
#if DEBUG
|> Program.withDebugger
#endif
|> Program.run
