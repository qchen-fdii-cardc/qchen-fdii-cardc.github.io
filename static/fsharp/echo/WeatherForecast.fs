namespace echo

open System
open System.ComponentModel.DataAnnotations

type WeatherForecast =
    { Date: DateTime
      TemperatureC: int
      Summary: string }

    member this.TemperatureF =
        32.0 + (float this.TemperatureC / 0.5556)

type AddResult =
    { Op1: float
      Op2: float
      Sum: float }

// Database entities
[<CLIMutable>]
type TodoItem =
    { [<Key>]
      Id: int
      Title: string
      IsCompleted: bool
      CreatedAt: DateTime
      UpdatedAt: DateTime option }

[<CLIMutable>]
type User =
    { [<Key>]
      Id: int
      Name: string
      Email: string
      CreatedAt: DateTime }
