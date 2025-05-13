namespace MouseDrawing

open System
open System.Drawing
open System.Windows.Forms
open System.Text.Json
open System.IO
open System.Diagnostics
open System.Text.Json.Serialization

open Types
open Converters

module MainWindow =
    
    let rec optionMap  fn objOrNull=
        objOrNull
        |> Option.ofObj
        |> Option.map fn
        |> ignore

    type DrawingForm() =
        inherit Form()
        
        // 字段定义
        let mutable lines = List.empty<FreeLine>
        let mutable settings = { Color = Color.Green; Width = 2; Style = Solid }
        let mutable isDrawing = false
        let mutable currentPoints = List.empty<Point>
        let mutable lastExportFolder = Environment.GetFolderPath(Environment.SpecialFolder.MyPictures)
        let mutable currentDpi = 600

        // 创建 JSON 序列化选项
        let jsonOptions = JsonSerializerOptions()
        do
            jsonOptions.Converters.Add(LineStyleConverter())
            jsonOptions.Converters.Add(ColorConverter())
            jsonOptions.Converters.Add(PointConverter())
            jsonOptions.Converters.Add(SizeConverter())

        // 主构造函数
        do
            base.SetStyle(
                ControlStyles.DoubleBuffer
                ||| ControlStyles.UserPaint
                ||| ControlStyles.AllPaintingInWmPaint,
                true
            )
            base.StartPosition <- FormStartPosition.CenterScreen
            base.Text <- "线条绘图绘大师 by F#"
            base.Size <- Size(800, 600)
            base.AllowDrop <- true

        // 初始化事件处理
        member this.installEvents() =
            this.MouseDown.Add(this.HandleMouseDown)
            this.MouseMove.Add(this.HandleMouseMove)
            this.MouseUp.Add(this.HandleMouseUp)
            this.Paint.Add(this.HandlePaint)
            this.KeyPreview <- true
            this.KeyDown.Add(this.HandleKeyDown)
            this.DragEnter.Add(this.HandleDragEnter)
            this.DragDrop.Add(this.HandleDragDrop)
            this

        // 拖放事件处理方法
        member private this.HandleDragEnter(e: DragEventArgs) =
            e.Data
            |> optionMap (fun dataObject ->
                    if dataObject.GetDataPresent(DataFormats.FileDrop) then
                        e.Effect <- DragDropEffects.Copy
                )
        member private this.SetLastExportFolderFromFile (file: string) =
            Path.GetDirectoryName (file)
            |> optionMap (fun dir -> lastExportFolder <- dir)            

        member private this.HandleDragDrop(e: DragEventArgs) =
            e.Data
            |> optionMap (fun data ->
                    data.GetData(DataFormats.FileDrop) :?> string array | null
                    |> optionMap (fun files ->
                            if files.Length > 0 && not (String.IsNullOrEmpty(files.[0])) then 
                                Path.GetExtension(files.[0])
                                |> optionMap (function
                                    | file when file.ToLower() = ".json" ->
                                        if File.ReadAllText(file)|> this.DeserializeDrawing then
                                            this.SetLastExportFolderFromFile file       
                                    | file when List.contains (file.ToLower()) [".png"; ".jpg" ; ".jpeg"] ->
                                        let background = new Bitmap(file)
                                        this.BackgroundImage <- background
                                        this.BackgroundImageLayout <- ImageLayout.Stretch
                                        this.Invalidate()
                                        this.SetLastExportFolderFromFile file                                        
                                    | _ -> ()
                                    )
                        )
                )            

        // 设置相关方法
        member this.SetColor color =
            settings <- { settings with Color = color }

        member this.SetWidth width =
            settings <- { settings with Width = width }

        member this.SetStyle style =
            settings <- { settings with Style = style }

        // 绘制相关方法
        member private this.DrawLine (graphics: Graphics) (points: Point[]) (settings: DrawSettings) =
            if points.Length > 1 then
                let pen =
                    let p = new Pen(settings.Color, float32 settings.Width)
                    p.DashStyle <-
                        match settings.Style with
                        | Solid -> Drawing2D.DashStyle.Solid
                        | Dash -> Drawing2D.DashStyle.Dash
                        | Dot -> Drawing2D.DashStyle.Dot
                        | DashDot -> Drawing2D.DashStyle.DashDot
                    p
                use _ = pen
                graphics.DrawLines(pen, points)

        member private this.DrawLines(graphics: Graphics) =
            lines
            |> List.rev
            |> List.iter (fun line -> this.DrawLine graphics (List.toArray line.Points) line.Settings)
            this.DrawLine graphics (List.toArray currentPoints) settings

        // 事件处理方法
        member private this.HandleMouseDown e =
            match e.Button with
            | MouseButtons.Left ->
                isDrawing <- true
                currentPoints <- [ e.Location ]
            | MouseButtons.Right ->
                let menu: ContextMenuStrip = this.CreateContextMenu()
                menu.Show(this, e.Location)
            | _ -> ()

        member private this.HandleMouseMove e =
            if isDrawing then
                currentPoints <- currentPoints @ [ e.Location ]
                this.Invalidate()

        member private this.HandleMouseUp e =
            if e.Button = MouseButtons.Left && isDrawing then
                isDrawing <- false
                if List.length currentPoints > 1 then
                    let newLine = {
                        Points = currentPoints
                        Settings = settings
                    }
                    lines <- newLine :: lines
                currentPoints <- []
                this.Invalidate()

        member private this.HandlePaint e =
            this.DrawLines e.Graphics

        member private this.HandleKeyDown(e: KeyEventArgs) =
            if e.Control then
                match e.KeyCode with
                | Keys.S ->
                    e.Handled <- true
                    this.SaveDrawing()
                | Keys.L ->
                    e.Handled <- true
                    this.LoadDrawing()
                | Keys.P ->
                    e.Handled <- true
                    this.ExportImage currentDpi
                | _ -> ()

        // 菜单相关方法
        member private this.CreateMenuItem (name: string) (action: unit -> unit) (isChecked: bool) =
            let item = new ToolStripMenuItem(name)
            item.Click.Add(fun _ -> action ())
            if isChecked then
                item.Checked <- true
            item

        member private this.CreateColorMenu (currentColor: Color) (setColor: Color -> unit) =
            let colorMenu = new ToolStripMenuItem("颜色")
            [ ("黑色", Color.Black);
            ("红色", Color.Red);
            ("绿色", Color.Green);
            ("蓝色", Color.Blue);
            ("黄色", Color.Yellow);
            ("紫色", Color.Purple);
            ("橙色", Color.Orange);
            ("棕色", Color.Brown);
            ("灰色", Color.Gray);
            ("粉色", Color.Pink) ]
            |> List.iter (fun (name, color) ->
                let item = this.CreateMenuItem name (fun _ -> setColor color) (color = currentColor)
                colorMenu.DropDownItems.Add(item) |> ignore)

            let customItem = new ToolStripMenuItem("自定义颜色...")
            customItem.Click.Add(fun _ ->
                use dlg = new ColorDialog()
                dlg.Color <- currentColor
                if dlg.ShowDialog() = DialogResult.OK then
                    setColor dlg.Color)
            colorMenu.DropDownItems.Add(customItem) |> ignore
            colorMenu

        member private this.CreateWidthMenu (currentWidth: int) (setWidth: int -> unit) =
            let widthMenu = new ToolStripMenuItem("宽度")
            [ ("1px", 1); ("2px", 2); ("3px", 3) ]
            |> List.iter (fun (name, width) ->
                let item = this.CreateMenuItem name (fun _ -> setWidth width) (width = currentWidth)
                widthMenu.DropDownItems.Add(item) |> ignore)
            widthMenu

        member private this.CreateStyleMenu (currentStyle: LineStyle) (setStyle: LineStyle -> unit) =
            let styleMenu = new ToolStripMenuItem("样式")
            [ ("实线", Solid); ("虚线", Dash); ("点线", Dot); ("点划线", DashDot) ]
            |> List.iter (fun (name, style) ->
                let item = this.CreateMenuItem name (fun _ -> setStyle style) (style = currentStyle)
                styleMenu.DropDownItems.Add(item) |> ignore)
            styleMenu

        member private this.CreateExportMenu() =
            let exportMenu = new ToolStripMenuItem("导出")
            [ ("300 DPI", 300); ("600 DPI", 600); ("1200 DPI", 1200) ]
            |> List.iter (fun (name, dpi) ->
                let item =
                    this.CreateMenuItem
                        name
                        (fun _ ->
                            currentDpi <- dpi
                            this.ExportImage dpi)
                        (dpi = currentDpi)
                exportMenu.DropDownItems.Add(item) |> ignore)
            exportMenu

        member private this.CreateContextMenu() =
            let menu = new ContextMenuStrip()
            menu.Items.Add(this.CreateColorMenu settings.Color this.SetColor) |> ignore
            menu.Items.Add(this.CreateWidthMenu settings.Width this.SetWidth) |> ignore
            menu.Items.Add(this.CreateStyleMenu settings.Style this.SetStyle) |> ignore
            menu.Items.Add(this.CreateExportMenu()) |> ignore
            menu.Items.Add(new ToolStripSeparator()) |> ignore
            menu.Items.Add(this.CreateMenuItem "保存绘图" this.SaveDrawing false) |> ignore
            menu.Items.Add(this.CreateMenuItem "加载绘图" this.LoadDrawing false) |> ignore
            menu.Items.Add(new ToolStripSeparator()) |> ignore
            menu.Items.Add(this.CreateMenuItem "打开导出文件夹" this.OpenExportFolder false) |> ignore
            menu.Items.Add(new ToolStripSeparator()) |> ignore
            menu.Items.Add(this.CreateMenuItem "重置" this.Reset false) |> ignore
            menu.Items.Add(this.CreateMenuItem "加载背景" this.LoadBackground false) |> ignore
            menu

        // 文件操作相关方法
        member private this.LoadBackground() =
            let openDialog = new OpenFileDialog()
            openDialog.Filter <-
                "Image files (*.png;*.jpg;*.jpeg)|*.png;*.jpg;*.jpeg|PNG files (*.png)|*.png|JPEG files (*.jpg;*.jpeg)|*.jpg;*.jpeg"
            openDialog.Title <- "加载背景"
            openDialog.InitialDirectory <- lastExportFolder
            
            if openDialog.ShowDialog() = DialogResult.OK then
                let background = new Bitmap(openDialog.FileName)
                this.BackgroundImage <- background
                this.BackgroundImageLayout <- ImageLayout.Stretch
                this.Invalidate()

        member this.ExportImage(dpi: int) =
            let saveDialog = new SaveFileDialog()
            saveDialog.Filter <- "PNG files (*.png)|*.png"
            saveDialog.Title <- "导出图像"
            saveDialog.InitialDirectory <- lastExportFolder

            if saveDialog.ShowDialog() = DialogResult.OK then
                // 考虑 DPI 缩放
                let scale = float32 dpi / 96.0f
                let width = int(float32 this.ClientSize.Width * scale)
                let height = int(float32 this.ClientSize.Height * scale)
                
                let bitmap = new Bitmap(width, height)
                use graphics = Graphics.FromImage(bitmap)
                
                // 设置高质量绘图
                graphics.SmoothingMode <- Drawing2D.SmoothingMode.AntiAlias
                graphics.InterpolationMode <- Drawing2D.InterpolationMode.HighQualityBicubic
                graphics.PixelOffsetMode <- Drawing2D.PixelOffsetMode.HighQuality
                
                // 应用 DPI 缩放
                graphics.ScaleTransform(scale, scale)
                
                // 绘制背景
                this.BackgroundImage
                |> optionMap (fun bg ->
                    graphics.DrawImage(bg, 0, 0, this.ClientSize.Width, this.ClientSize.Height)
                )
                
                // 绘制线条
                this.DrawLines graphics
                
                bitmap.Save(saveDialog.FileName, Imaging.ImageFormat.Png)
                this.SetLastExportFolderFromFile saveDialog.FileName

        member private this.OpenExportFolder() =
            Process.Start("explorer.exe", lastExportFolder) |> ignore

        member private this.Reset() =
            lines <- List.empty<FreeLine>
            isDrawing <- false
            currentPoints <- List.empty<Point>
            this.BackgroundImage <- null
            this.Invalidate()

        member private this.SerializeDrawing() =
            let data = {
                Lines = lines
                Settings = settings
                FormSize = this.Size
            }
            JsonSerializer.Serialize(data, jsonOptions)

        member private this.DeserializeDrawing(json: string) =
            JsonSerializer.Deserialize<DrawingData>(json, jsonOptions)
            |> optionMap (fun data ->
                lines <- data.Lines
                settings <- data.Settings
                this.Size <- data.FormSize
                this.Invalidate()                
            )
            true

        member private this.SaveDrawing() =
            let saveDialog = new SaveFileDialog()
            saveDialog.Filter <- "JSON files (*.json)|*.json"
            saveDialog.Title <- "Save Drawing"
            saveDialog.InitialDirectory <- lastExportFolder
            if saveDialog.ShowDialog() = DialogResult.OK then
                let json = this.SerializeDrawing()
                File.WriteAllText(saveDialog.FileName, json)
                this.SetLastExportFolderFromFile saveDialog.FileName

        member private this.LoadDrawing() =
            let openDialog = new OpenFileDialog()
            openDialog.Filter <- "JSON files (*.json)|*.json"
            openDialog.Title <- "Load Drawing"
            openDialog.InitialDirectory <- lastExportFolder
            if openDialog.ShowDialog() = DialogResult.OK &&
                File.ReadAllText(openDialog.FileName) |> this.DeserializeDrawing then
                    this.SetLastExportFolderFromFile openDialog.FileName