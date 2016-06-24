module CarpaccioModel
  open System

  [<CLIMutable>]
  type Result = {total: double}
  
  [<CLIMutable>]
  type Feedback =
    { Type: string
      Content: string }

  [<CLIMutable>]
  type Order =
      { prices : double[]
        quantities: double[]
        country: string
        reduction: string
      }
