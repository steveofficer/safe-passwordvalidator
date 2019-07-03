module ViewComponents
open Types
open Shared
open Fable.React
open Fable.React.Props
open Fulma

// This shows the current notification (if there is any)
let ``app-notification-message`` dispatch = function
    | None -> null
    | Some notification ->
        let (messageClass, message) = 
            match notification with
            | Error e -> "is-danger", e
            | Warning e -> "is-warning", e
            | Message m -> "is-success", m
        
        Notification.notification [ Notification.CustomClass messageClass ] [
            Notification.delete [ Props [ OnClick (fun _ -> dispatch HideNotification) ]] []
            str message
        ]

// This shows a single policy result
let ``app-policy-result`` = function
    | { Name = name; Result = Ok() } ->
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

// This shows the user the break down of which policies have been met and which have not been met
let ``app-password-status`` model =
    Section.section [] [
        (match model with
            | None -> str "Nothing entered yet 👎"
            | Some value ->
                match value with
                | ValidPassword _ -> str "That's a valid password 👍"
                | InvalidPassword (_, policies) -> div [] [ for policy in policies -> ``app-policy-result`` policy ]
        )
    ]                