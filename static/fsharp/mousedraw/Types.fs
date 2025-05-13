namespace MouseDrawing

open System.Drawing

module Types =
    type LineStyle =
        | Solid
        | Dash
        | Dot
        | DashDot

    type DrawSettings =
        { Color: Color
          Width: int
          Style: LineStyle }

    type FreeLine =
        { Points: Point list
          Settings: DrawSettings }

    type DrawingData =
        { Lines: FreeLine list
          Settings: DrawSettings
          FormSize: Size }
