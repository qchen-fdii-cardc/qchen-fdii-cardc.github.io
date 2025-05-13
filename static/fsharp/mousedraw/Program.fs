namespace MouseDrawing

open System.Windows.Forms
open System

open MainWindow

module Program =
    [<STAThread>]
    [<EntryPoint>]
    let main argv =
        Application.EnableVisualStyles()
        Application.SetCompatibleTextRenderingDefault(false)
        let form = new DrawingForm()
        Application.Run(form.installEvents ())
        0
