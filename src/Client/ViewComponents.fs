module ViewComponents
open Types
open Shared
open Fable.React
open Fable.React.Props
open Fulma

// This shows the current notification (if there is any)
let ``notification message`` dispatch = function
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

// This shows the current password status to the user
// If there is an invalid password then it shows which policies
// they have met and which they haven't yet met.
let ``password status`` password =
    let ``policy result`` (policy: PolicyResult) =
        match policy with
        | { Name = name; Result = Ok ()} ->
            div [] [
                str name
                span [ Class "icon has-text-success"] [ i [ Class "fas fa-check-square" ] [] ]
            ]
        | { Name = name; Result = Result.Error err } ->
            div [] [
                str name
                //span [ Class "icon has-text-warning"] [ i [ Class "fas fa-exclamation-triangle" ] [] ]
                p [ Class "tag is-warning"] [ str err ]
            ]

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