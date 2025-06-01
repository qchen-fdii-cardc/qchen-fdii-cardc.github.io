namespace echo.Data

open Microsoft.EntityFrameworkCore
open echo

type AppDbContext(options: DbContextOptions<AppDbContext>) =
    inherit DbContext(options)

    [<DefaultValue>]
    val mutable private _todoItems: DbSet<TodoItem>
    member this.TodoItems
        with get() = this._todoItems
        and set v = this._todoItems <- v

    [<DefaultValue>]
    val mutable private _users: DbSet<User>
    member this.Users
        with get() = this._users
        and set v = this._users <- v

    override _.OnModelCreating(modelBuilder: ModelBuilder) =
        // Configure TodoItem
        modelBuilder.Entity<TodoItem>()
            .HasKey(fun x -> x.Id :> obj) |> ignore
        
        modelBuilder.Entity<TodoItem>()
            .Property(fun x -> x.Title)
            .IsRequired()
            .HasMaxLength(200) |> ignore

        // Configure User
        modelBuilder.Entity<User>()
            .HasKey(fun x -> x.Id :> obj) |> ignore
        
        modelBuilder.Entity<User>()
            .Property(fun x -> x.Email)
            .IsRequired()
            .HasMaxLength(100) |> ignore

        modelBuilder.Entity<User>()
            .HasIndex(fun x -> x.Email :> obj)
            .IsUnique() |> ignore

        base.OnModelCreating(modelBuilder) 