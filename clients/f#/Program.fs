module XCarpaccio.Program

open System
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Hosting

/// Exposes the entry point so WebApplicationFactory can host the app in tests.
type Program() =
    do ()

let port =
    Environment.GetEnvironmentVariable "PORT"
    |> Option.ofObj
    |> Option.defaultValue "3000"

let builder = WebApplication.CreateBuilder(Array.empty)
builder.WebHost.UseUrls($"http://0.0.0.0:{port}") |> ignore

let app = builder.Build()

app.MapPost("/ping", Func<string>(fun () -> "pong")) |> ignore

[<EntryPoint>]
let main _ =
    app.Run()
    0
