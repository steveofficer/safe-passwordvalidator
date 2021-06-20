module ViewComponents
open Types
open Shared
open Fable.React
open Fable.React.Props

// This shows the current notification (if there is any)
let ``notification message`` = FunctionComponent.Of(fun (props: {| dispatch: Msg -> unit; message: Notification option |}) ->
    match props.message with
    | None -> null
    | Some notification ->
        let (messageClass, message) =
            match notification with
            | ErrorMessage e -> "error", e
            | WarningMessage e -> "warning", e
            | SuccessMessage m -> "success", m

        span [ classList [ messageClass, true; "notification", true ] ] [ str message ]
)

// This shows a single policy result
let ``policy result`` =
    FunctionComponent.Of(
        fun (props: PolicyResult) ->
            div [ ClassName "policy"]
                (match props with
                | { Name = name; Result = Ok() } ->
                    [
                        span [ Class "icon success"] [ i [ Class "fas fa-check-square" ] [] ]
                        span [] [ str name ]
                    ]
                | { Name = name; Result = Error err } ->
                    [
                        span [ Class "icon danger"] [ i [ Class "fas fa-times-circle" ] [] ]
                        span [] [ str name ]
                        span [ Class "tag is-warning" ] [ str err ]
                    ])
        , withKey = (fun p -> p.Name))

let skeleton = div [ Class "skeleton" ] []