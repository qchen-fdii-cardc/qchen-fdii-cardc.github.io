namespace echo.Controllers

open System
open System.Linq
open System.Threading.Tasks
open Microsoft.AspNetCore.Mvc
open Microsoft.Extensions.Logging
open Microsoft.EntityFrameworkCore
open echo
open echo.Data

[<CLIMutable>]
type CreateTodoRequest =
    { Title: string }

[<CLIMutable>]
type UpdateTodoRequest =
    { Title: string
      IsCompleted: bool }

[<ApiController>]
[<Route("api/[controller]")>]
type TodoController(logger: ILogger<TodoController>, context: AppDbContext) =
    inherit ControllerBase()

    /// <summary>
    /// Gets all todo items
    /// </summary>
    /// <returns>List of todo items</returns>
    [<HttpGet>]
    [<ProducesResponseType(typeof<TodoItem[]>, 200)>]
    member _.GetTodos() =
        task {
            let! todos = context.TodoItems.ToArrayAsync()
            return todos
        }

    /// <summary>
    /// Gets a specific todo item by ID
    /// </summary>
    /// <param name="id">Todo item ID</param>
    /// <returns>Todo item or 404 if not found</returns>
    [<HttpGet("{id}")>]
    [<ProducesResponseType(typeof<TodoItem>, 200)>]
    [<ProducesResponseType(404)>]
    member this.GetTodo(id: int) =
        task {
            let! todo = context.TodoItems.FindAsync(id).AsTask()
            if isNull todo then
                return this.NotFound() :> IActionResult
            else
                return this.Ok(todo) :> IActionResult
        }

    /// <summary>
    /// Creates a new todo item
    /// </summary>
    /// <param name="request">Todo creation request</param>
    /// <returns>Created todo item</returns>
    [<HttpPost>]
    [<ProducesResponseType(typeof<TodoItem>, 201)>]
    [<ProducesResponseType(400)>]
    member this.CreateTodo([<FromBody>] request: CreateTodoRequest) =
        task {
            if String.IsNullOrWhiteSpace(request.Title) then
                return this.BadRequest("Title is required") :> IActionResult
            else
                let newTodo = {
                    Id = 0 // EF will auto-generate
                    Title = request.Title.Trim()
                    IsCompleted = false
                    CreatedAt = DateTime.UtcNow
                    UpdatedAt = None
                }
                
                context.TodoItems.Add(newTodo) |> ignore
                let! _ = context.SaveChangesAsync()
                
                return this.CreatedAtAction(nameof(this.GetTodo), {| id = newTodo.Id |}, newTodo) :> IActionResult
        }

    /// <summary>
    /// Updates an existing todo item
    /// </summary>
    /// <param name="id">Todo item ID</param>
    /// <param name="request">Update request</param>
    /// <returns>Updated todo item or 404 if not found</returns>
    [<HttpPut("{id}")>]
    [<ProducesResponseType(typeof<TodoItem>, 200)>]
    [<ProducesResponseType(400)>]
    [<ProducesResponseType(404)>]
    member this.UpdateTodo(id: int, [<FromBody>] request: UpdateTodoRequest) =
        task {
            let! existingTodo = context.TodoItems.FindAsync(id).AsTask()
            if isNull existingTodo then
                return this.NotFound() :> IActionResult
            else
                if String.IsNullOrWhiteSpace(request.Title) then
                    return this.BadRequest("Title is required") :> IActionResult
                else
                    let updatedTodo = {
                        existingTodo with
                            Title = request.Title.Trim()
                            IsCompleted = request.IsCompleted
                            UpdatedAt = Some DateTime.UtcNow
                    }
                    
                    context.Entry(existingTodo).CurrentValues.SetValues(updatedTodo)
                    let! _ = context.SaveChangesAsync()
                    
                    return this.Ok(updatedTodo) :> IActionResult
        }

    /// <summary>
    /// Deletes a todo item
    /// </summary>
    /// <param name="id">Todo item ID</param>
    /// <returns>204 No Content or 404 if not found</returns>
    [<HttpDelete("{id}")>]
    [<ProducesResponseType(204)>]
    [<ProducesResponseType(404)>]
    member this.DeleteTodo(id: int) =
        task {
            let! todo = context.TodoItems.FindAsync(id).AsTask()
            if isNull todo then
                return this.NotFound() :> IActionResult
            else
                context.TodoItems.Remove(todo) |> ignore
                let! _ = context.SaveChangesAsync()
                return this.NoContent() :> IActionResult
        } 