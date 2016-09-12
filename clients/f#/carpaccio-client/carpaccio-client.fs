module CarpaccioClient
open Suave
open Suave.Filters
open Suave.Operators
open Suave.Successful
open System.Net
open System.Text
open Newtonsoft.Json
open CarpaccioModel

let decodeUTF8ContentToString (request:HttpRequest) =
    Encoding.UTF8.GetString request.rawForm

let deserialize<'a> (s:string) : 'a option = 
    try
        Some (JsonConvert.DeserializeObject<'a> s)
    with
        _ -> printfn "ERROR can't deserialize : %s" s
             None

let handleFeedback (r:HttpRequest) =
    let feedback = decodeUTF8ContentToString >> deserialize<Feedback> <| r
    printfn "Feedback -> %A" <| feedback
    OK ""

let handleOrder (r:HttpRequest) =
    let order = decodeUTF8ContentToString >> deserialize<Order> <| r
    printfn "order received -> %A" order
    OK ""

let app =
    choose [
        GET >=>  choose
            [ path "/order" >=> OK "Hello World!"
              path "/ping" >=> OK "pong" ]
        POST >=> choose
            [ path "/order" >=> request ( fun r -> r |> handleOrder )
              path "/feedback" >=> request (fun r -> r |> handleFeedback ) ]
           ]

let port = Sockets.Port.Parse <| "8080"
let ip = IPAddress.Any

let serverConfig =
    { defaultConfig with
       bindings = [ HttpBinding.mk HTTP ip port ]
    }

[<EntryPoint>]
let main argv =
    printfn "Starting server ..."
    startWebServer serverConfig app
    0
