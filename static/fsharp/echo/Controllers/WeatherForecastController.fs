namespace echo.Controllers

open System
open System.Collections.Generic
open System.Linq
open System.Threading.Tasks
open Microsoft.AspNetCore.Mvc
open Microsoft.Extensions.Logging
open echo

[<ApiController>]
[<Route("[controller]")>]
type WeatherForecastController (logger : ILogger<WeatherForecastController>) =
    inherit ControllerBase()

    let summaries =
        [|
            "Freezing"
            "Bracing"
            "Chilly"
            "Cool"
            "Mild"
            "Warm"
            "Balmy"
            "Hot"
            "Sweltering"
            "Scorching"
        |]

    /// <summary>
    /// Gets a random weather forecast for the next 5 days
    /// </summary>
    /// <returns>An array of weather forecasts</returns>
    [<HttpGet>]
    [<ProducesResponseType(typeof<WeatherForecast[]>, 200)>]
    member _.Get() =
        let rng = System.Random()
        [|
            for index in 0..4 ->
                { Date = DateTime.Now.AddDays(float index)
                  TemperatureC = rng.Next(-20,55)
                  Summary = summaries.[rng.Next(summaries.Length)] }
        |]

    /// <summary>
    /// Adds two numbers and returns the result
    /// </summary>
    /// <param name="op1">First operand</param>
    /// <param name="op2">Second operand</param>
    /// <returns>The result containing both operands and their sum</returns>
    [<HttpGet("add")>]
    [<ProducesResponseType(typeof<AddResult>, 200)>]
    member _.Add([<FromQuery>] op1: float, [<FromQuery>] op2: float) =
        { Op1 = op1
          Op2 = op2
          Sum = op1 + op2 }
