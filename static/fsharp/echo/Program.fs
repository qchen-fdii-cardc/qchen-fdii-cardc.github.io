namespace echo
#nowarn "20"
open System
open System.Collections.Generic
open System.IO
open System.Linq
open System.Threading.Tasks
open Microsoft.AspNetCore
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Hosting
open Microsoft.AspNetCore.HttpsPolicy
open Microsoft.Extensions.Configuration
open Microsoft.Extensions.DependencyInjection
open Microsoft.Extensions.Hosting
open Microsoft.Extensions.Logging
open Microsoft.EntityFrameworkCore
open echo.Data

module Program =
    let exitCode = 0

    [<EntryPoint>]
    let main args =

        let builder = WebApplication.CreateBuilder(args)

        builder.Services.AddControllers() |> ignore
        
        // Add Entity Framework and SQLite
        builder.Services.AddDbContext<AppDbContext>(fun options ->
            options.UseSqlite(builder.Configuration.GetConnectionString("DefaultConnection")) |> ignore
        ) |> ignore
        
        // Add Swagger/OpenAPI services
        builder.Services.AddEndpointsApiExplorer() |> ignore
        builder.Services.AddSwaggerGen() |> ignore

        let app = builder.Build()

        // Ensure database is created
        use scope = app.Services.CreateScope()
        let context = scope.ServiceProvider.GetRequiredService<AppDbContext>()
        context.Database.EnsureCreated() |> ignore

        // Configure Swagger middleware (only in Development)
        if app.Environment.IsDevelopment() then
            app.UseSwagger() |> ignore
            app.UseSwaggerUI() |> ignore

        app.UseHttpsRedirection() |> ignore

        app.UseAuthorization() |> ignore
        app.MapControllers() |> ignore

        app.Run()

        exitCode
