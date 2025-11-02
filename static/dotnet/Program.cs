using System;
using System.Reflection;
public static class AssemblyInspector
{
    public static void PrintAssemblyTypesAndMembers(Assembly assembly)
    {
        foreach (var type in assembly.GetTypes())
        {
            Console.WriteLine($"Type: {type.FullName}");
            foreach (var method in type.GetMethods(BindingFlags.Public | BindingFlags.NonPublic | BindingFlags.Instance | BindingFlags.Static | BindingFlags.DeclaredOnly))
            {
                var parameters = string.Join(", ", method.GetParameters().Select(p => $"{p.ParameterType.Name} {p.Name}"));
                Console.WriteLine($"    Method: {method.ReturnType.Name} {method.Name}({parameters})");
            }
            foreach (var property in type.GetProperties(BindingFlags.Public | BindingFlags.NonPublic | BindingFlags.Instance | BindingFlags.Static | BindingFlags.DeclaredOnly))
            {
                Console.WriteLine($"  Property: {property.PropertyType.Name} {property.Name}");
            }
        }
    }
}


public sealed class Program
{
    public static void Main(string[] args)
    {
        var consoleAssembly = typeof(Console).Assembly;
        AssemblyInspector.PrintAssemblyTypesAndMembers(consoleAssembly);

        var systemRuntimeAssembly = Assembly.Load("System.Runtime");
        AssemblyInspector.PrintAssemblyTypesAndMembers(systemRuntimeAssembly);
    }
}