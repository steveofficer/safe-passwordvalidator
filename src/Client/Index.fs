module Index

open Elmish
open Fable.Remoting.Client
open Shared
open Types

Fable.Core.JsInterop.importAll "./main.scss"

// An active pattern which helps to make match statements more explicit about what is being matched against
let (|Empty|NotEmpty|) = function | "" -> Empty | x -> NotEmpty x

let api =
    Remoting.createApi()
    |> Remoting.withRouteBuilder Route.builder
    |> Remoting.buildProxy<IApi>

// This function defines the initial state and initial command (a.k.a side-effect) of the application
let init(): Model * Cmd<Msg> =
    // Create the inital model with default values
    let initialModel = { Password = None; Notification = None; ServerInfo = None }

    // Call the getInfo api method then dispath the result using the SetServerInfo message
    let initialCommand =
        Cmd.OfAsync.perform
            (fun () -> async {
                return! api.getInfo()
            })
            ()
            SetServerInfo

    initialModel, initialCommand

// The update function computes the next state of the application based on the current state and the incoming events/messages
// It can also run side-effects (encoded as commands) like calling the server via Http.
// These commands in turn, can dispatch messages to which the update function will react.
let update (msg: Msg) (currentModel: Model) =
    match msg with

        | SetPassword Empty -> { currentModel with Password = None; Notification = None }, Cmd.none

        | SetPassword (NotEmpty p) -> { currentModel with Password = p |> Validator.validate |> Some; Notification = None }, Cmd.none

        | SavePassword ->
            match currentModel.Password with
            | None -> { currentModel with Notification = "No password has been entered" |> WarningMessage |> Some }, Cmd.none
            | Some (ValidPassword password) ->
                let cmd =
                     Cmd.OfAsync.perform
                         api.save password
                         (function
                            | Ok() -> "The server accepted the password" |> SuccessMessage |> ShowNotification
                            | Error message -> message |> ErrorMessage |> ShowNotification)
                currentModel, cmd
            | Some (InvalidPassword _) ->
                let currentModel = { currentModel with Notification = "An invalid password cannot be saved" |> WarningMessage |> Some }
                let cmd = Cmd.none
                currentModel, cmd

        | ShowNotification notification -> { currentModel with Notification = Some notification }, Cmd.none

        | HideNotification -> { currentModel with Notification = None }, Cmd.none

        | SetServerInfo info -> { currentModel with ServerInfo = Some info }, Cmd.none

open Fable.React
open Fable.React.Props
open ViewComponents

let view (model : Model) (dispatch : Msg -> unit) =
    div [ Class "container" ] [
        h3 [] [ str "Password Validator" ]

        div [ Class "content" ] [
            form [] [
                div [ Class "fieldcontrol"] [
                    label [ HtmlFor "password" ] [ str "Password" ]
                    input [ Id "password"; AutoFocus true; AutoComplete "off"; Type "password"; OnChange(fun e -> dispatch (SetPassword e.Value)) ]
                ]

                button [
                    Class "primary"
                    OnClick(fun e ->
                        e.preventDefault()
                        dispatch SavePassword)
                    ]
                    [ str "Save" ]

                ``notification message`` {| dispatch = dispatch; message = model.Notification |}
            ]

            div [ Id "instruction" ] [
                (match model.Password with
                | None ->
                    div [ Id "blankPasswordMessage"] [
                        p [] [ str "There are a number of policies that your password must meet." ]
                        p [] [ str "As you enter your password you will see policies have been met and which ones have not." ]
                    ]
                | Some (ValidPassword _) -> div [ Id "validPasswordMessage" ] [ str "That's a valid password 👍" ]
                | Some (InvalidPassword (_, policies)) -> div [ Id "policies" ] (policies |> List.map ``policy result``))
            ]
        ]

        footer []
            (match model.ServerInfo with
            | None ->
                [ skeleton; skeleton ]
            | Some info ->
                [
                    p [] [ info.Version |> str ]
                    p [] [ info.Time.ToString() |> str ]
                ])
    ]
