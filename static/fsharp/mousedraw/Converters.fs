namespace MouseDrawing

open System
open System.Text.Json
open System.Text.Json.Serialization
open System.Drawing
open Types

module Converters =
    type LineStyleConverter() =
        inherit JsonConverter<LineStyle>()

        override _.Read(reader: byref<Utf8JsonReader>, typeToConvert: Type, options: JsonSerializerOptions) =
            let value = reader.GetString()

            match value with
            | "Solid" -> Solid
            | "Dash" -> Dash
            | "Dot" -> Dot
            | "DashDot" -> DashDot
            | _ -> Solid

        override _.Write(writer: Utf8JsonWriter, value: LineStyle, options: JsonSerializerOptions) =
            let str =
                match value with
                | Solid -> "Solid"
                | Dash -> "Dash"
                | Dot -> "Dot"
                | DashDot -> "DashDot"

            writer.WriteStringValue(str)

    type ColorConverter() =
        inherit JsonConverter<Color>()

        override _.Read(reader: byref<Utf8JsonReader>, typeToConvert: Type, options: JsonSerializerOptions) =
            let value = reader.GetInt32()
            Color.FromArgb(value)

        override _.Write(writer: Utf8JsonWriter, value: Color, options: JsonSerializerOptions) =
            writer.WriteNumberValue(value.ToArgb())

    type PointConverter() =
        inherit JsonConverter<Point>()

        override _.Read(reader: byref<Utf8JsonReader>, typeToConvert: Type, options: JsonSerializerOptions) =
            let mutable x = 0
            let mutable y = 0
            reader.Read() |> ignore

            while reader.TokenType <> JsonTokenType.EndObject do
                if reader.TokenType = JsonTokenType.PropertyName then
                    let propName = reader.GetString()
                    reader.Read() |> ignore

                    match propName with
                    | "X" -> x <- reader.GetInt32()
                    | "Y" -> y <- reader.GetInt32()
                    | _ -> ()

                reader.Read() |> ignore

            Point(x, y)

        override _.Write(writer: Utf8JsonWriter, value: Point, options: JsonSerializerOptions) =
            writer.WriteStartObject()
            writer.WriteNumber("X", value.X)
            writer.WriteNumber("Y", value.Y)
            writer.WriteEndObject()

    type SizeConverter() =
        inherit JsonConverter<Size>()

        override _.Read(reader: byref<Utf8JsonReader>, typeToConvert: Type, options: JsonSerializerOptions) =
            let mutable width = 0
            let mutable height = 0
            reader.Read() |> ignore

            while reader.TokenType <> JsonTokenType.EndObject do
                if reader.TokenType = JsonTokenType.PropertyName then
                    let propName = reader.GetString()
                    reader.Read() |> ignore

                    match propName with
                    | "Width" -> width <- reader.GetInt32()
                    | "Height" -> height <- reader.GetInt32()
                    | _ -> ()

                reader.Read() |> ignore

            Size(width, height)

        override _.Write(writer: Utf8JsonWriter, value: Size, options: JsonSerializerOptions) =
            writer.WriteStartObject()
            writer.WriteNumber("Width", value.Width)
            writer.WriteNumber("Height", value.Height)
            writer.WriteEndObject()
