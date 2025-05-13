# F# 绘图程序

这是一个使用 F# 和 Windows Forms 开发的简单绘图程序。

## 项目结构

- `Types.fs`: 定义核心数据类型和结构
- `Converters.fs`: 处理数据转换和序列化
- `Drawing.fs`: 实现绘图功能的核心逻辑
- `Program.fs`: 程序入口点
- `fsharp.fsproj`: 项目配置文件

## 重要的 F# 语言特性

### 1. 类型定义

```fsharp
// 可区分联合类型 (Discriminated Union)
type LineStyle =
    | Solid
    | Dash
    | Dot
    | DashDot

// 记录类型 (Record Type)
type DrawSettings =
    { Color: Color
      Width: int
      Style: LineStyle }
```

- 可区分联合类型用于表示一组互斥的选项
- 记录类型用于表示具有命名字段的数据结构

### 2. 类定义和继承

```fsharp
type MouseDrawingForm() =
    inherit Form()
    // 字段定义
    let mutable lines = List.empty<FreeLine>
    let mutable settings = { Color = Color.Green; Width = 2; Style = Solid }
```

- 使用 `inherit` 关键字进行继承
- 使用 `let mutable` 定义可变字段
- 使用 `member` 定义方法

### 3. 模式匹配

```fsharp
match e.Button with
| MouseButtons.Left ->
    isDrawing <- true
    currentPoints <- [e.Location]
| MouseButtons.Right ->
    this.CreateContextMenu()
    |> fun menu -> menu.Show(this, e.Location)
| _ -> ()
```

- 使用 `match` 表达式进行模式匹配
- 使用 `|>` 管道操作符进行函数组合

### 4. 函数式编程特性

```fsharp
[ ("黑色", Color.Black); ("红色", Color.Red); ("绿色", Color.Green) ]
|> List.iter (fun (name, color) ->
    let item = this.CreateMenuItem name (fun _ -> setColor color) (color = currentColor)
    colorMenu.DropDownItems.Add(item) |> ignore)
```

- 使用列表和管道操作符
- 使用高阶函数（如 `List.iter`）
- 使用 lambda 表达式

### 5. 事件处理

```fsharp
member this.installEvents() =
    this.MouseDown.Add(this.HandleMouseDown)
    this.MouseMove.Add(this.HandleMouseMove)
    this.MouseUp.Add(this.HandleMouseUp)
    this.Paint.Add(this.HandlePaint)
    this
```

- 使用 `Add` 方法注册事件处理程序
- 方法返回 `this` 支持链式调用

### 6. 不可变性和可变性

```fsharp
// 不可变列表
let points = [e.Location]

// 可变字段
let mutable isDrawing = false
```

- 默认情况下，F# 中的值是不可变的
- 使用 `mutable` 关键字声明可变值

### 7. 类型推断

```fsharp
let handleMouseDown e =  // 类型由编译器自动推断
    match e.Button with
    | MouseButtons.Left -> ...
```

- F# 具有强大的类型推断系统
- 通常不需要显式声明类型

## 运行要求

- .NET 9.0 或更高版本
- Windows 操作系统
- Windows Forms 支持

## 功能特性

- 自由绘制线条
- 自定义线条颜色、宽度和样式
- 导出图片（支持多种 DPI）
- 快捷键支持（Ctrl+E 快速导出）
- 右键菜单操作
- JSON 序列化支持（使用 System.Text.Json）
- 单文件发布支持
