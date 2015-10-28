open System
open System.Net
open Newtonsoft.Json

open Suave
open Suave.Http
open Suave.Http.Applicatives
open Suave.Http.Successful
open Suave.Web
open Suave.Types
open Suave.Utils

let deserialize<'a> s = JsonConvert.DeserializeObject<'a> s

[<CLIMutable>]
type Feedback = 
  { Type: string
    Content: string }

 
let handleFeedback request = 
    let feedback = 
        request.rawForm
        |> UTF8.toString
        |> deserialize<Feedback>

    Console.Write("Type: {0}: {1}", feedback.Type, feedback.Content);

    OK ""

let handleOrder request =
    let order = 
        request.rawForm
        |> UTF8.toString
    
    Console.WriteLine(order)

    OK ""

let app = 
    choose [
        GET >>= choose [
            path "order" >>= OK "Hello World!"
        ]
        POST >>= choose [
            path "/order" >>= request handleOrder
            path "/feedback" >>= request handleFeedback
        ]
    ]

let port = Sockets.Port.Parse <| "8080"
let ip = IPAddress.Loopback

let serverConfig = 
    { defaultConfig with
       bindings = [ HttpBinding.mk HTTP ip port ]
    }

[<EntryPoint>]
let main argv = 
    startWebServer serverConfig app
    0
