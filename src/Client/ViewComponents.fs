module ViewComponents
open Types
open Shared
open Fable.React
open Fable.React.Props
open Fulma

// This shows the current notification (if there is any)
let ``app-notification-message`` = FunctionComponent.Of((fun (props: {| message: Notification option; dispatch: Msg -> unit |}) ->
    match props.message with
    | None -> null
    | Some notification ->
        let (messageClass, message) = 
            match notification with
            | ErrorMessage e -> "is-danger", e
            | WarningMessage e -> "is-warning", e
            | SuccessMessage m -> "is-success", m
        
        Notification.notification [ Notification.CustomClass messageClass ] [
            Notification.delete [ Props [ OnClick (fun _ -> props.dispatch HideNotification) ]] []
            str message
        ]
), "NotificationMessage"
)

// This shows a single policy result
let ``app-policy-result`` = FunctionComponent.Of((fun (policy: PolicyResult) ->
    div [ Key policy.Name ] [
        str policy.Name
        (match policy.Result with
            | Ok() -> span [ Class "icon has-text-success"] [ i [ Class "fas fa-check-square" ] [] ]
            | Error err -> p [ Class "tag is-warning"] [ str err ]
        )
    ]
), "PolicyResult"
)

// This shows the user the break down of which policies have been met and which have not been met
let ``app-password-status`` = FunctionComponent.Of((fun (password: Password option) ->
    Section.section [] [
        (match password with
            | None -> str "Nothing entered yet 👎"
            | Some value ->
                match value with
                | ValidPassword _ -> str "That's a valid password 👍"
                | InvalidPassword (_, policies) -> div [ ] [ for policy in policies -> ``app-policy-result`` policy ]
        )
    ]
), "PasswordStatus"
)