# SAFE Password Policy Validator

This project is a simple webapp that asks a user to enter a password and then runs the password through a policy validator and lets the user know if they password is valid, or, which policy rules the password has not met.

Any rules that fail are displayed to the user.

This highlights how F# can be used to create a full stack application using ASP.NET Core as a Backend and React as a frontend, with both tiers sharing the same F# based domain logic without it needing to be rewritten.

It also illustrates how F# and functional principles can be used to compose an easily maintainable and extensible set of rules that validate a domain model.


There are 3 main branches that take the application through various stages

`https://github.com/steveofficer/safe-passwordvalidator/tree/demo/boolean-results/src` shows how the code would work if policy rules just return boolean values, like you would perhaps do in a C# program.

`https://github.com/steveofficer/safe-passwordvalidator/blob/master/src` shows how we can use Discriminated Unions to improve the model by allowing the policy rules to either return `unit` or return an error message. This means that when a policy fails, we not only know that it failed, but also why it failed.

`https://github.com/steveofficer/safe-passwordvalidator/tree/demo/full-react` then extends the UI portion to show how to transition the UI components from F# functions to React Functional Components.


## Install pre-requisites

This application was created using the dotnet [SAFE Template](https://safe-stack.github.io/docs/template-overview/).

You'll need to install the following pre-requisites in order to build SAFE applications

* The [.NET Core SDK](https://www.microsoft.com/net/download)
* [FAKE 5](https://fake.build/) installed as a [global tool](https://fake.build/fake-gettingstarted.html#Install-FAKE)
* The [Yarn](https://yarnpkg.com/lang/en/docs/install/) package manager (you an also use `npm` but the usage of `yarn` is encouraged).
* [Node LTS](https://nodejs.org/en/download/) installed for the front end components.
* If you're running on OSX or Linux, you'll also need to install [Mono](https://www.mono-project.com/docs/getting-started/install/).

## Work with the application

To concurrently run the server and the client components in watch mode use the following command:

```bash
fake build -t Run
```


## SAFE Stack Documentation

You will find more documentation about the used F# components at the following places:

* [Saturn](https://saturnframework.org/docs/)
* [Fable](https://fable.io/docs/)
* [Elmish](https://elmish.github.io/elmish/)
* [Fulma](https://fulma.github.io/Fulma/)

If you want to know more about the full Azure Stack and all of it's components (including Azure) visit the official [SAFE documentation](https://safe-stack.github.io/docs/).

## Troubleshooting

* **fake not found** - If you fail to execute `fake` from command line after installing it as a global tool, you might need to add it to your `PATH` manually: (e.g. `export PATH="$HOME/.dotnet/tools:$PATH"` on unix) - [related GitHub issue](https://github.com/dotnet/cli/issues/9321)
