module XCarpaccio.Tests.PingTests

open System.Net
open Microsoft.AspNetCore.Mvc.Testing
open Xunit

let factory = new WebApplicationFactory<XCarpaccio.Program.Program>()

[<Fact>]
let ``POST /ping responds with pong`` () =
    task {
        use client = factory.CreateClient()
        use! response = client.PostAsync("/ping", null)
        let! body = response.Content.ReadAsStringAsync()

        Assert.Equal(HttpStatusCode.OK, response.StatusCode)
        Assert.Equal("pong", body)
    }
