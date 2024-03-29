module Types
open Shared

// The model holds data that you want to keep track of while the application is running
// We keep track of the password as well as any message being displayed to the user
type Model = {
    Password: Password option
    Notification: Notification option
    ServerInfo: ServerInfo option
}
and Notification =
    | ErrorMessage of string
    | WarningMessage of string
    | SuccessMessage of string

// The Msg type defines what events/actions can occur while the application is running
// the state of the application changes *only* in reaction to these events
type Msg =
| SetPassword of string
| SavePassword
| ShowNotification of Notification
| HideNotification
| SetServerInfo of ServerInfo