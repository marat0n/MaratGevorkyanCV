#r "nuget: Microsoft.Playwright"

open Microsoft.Playwright
open System.IO
open System
open System.Threading.Tasks



let awaitTask task = task |> Async.AwaitTask |> Async.RunSynchronously
let awaitUnitTask (task: Task) = task |> Async.AwaitTask |> Async.RunSynchronously

let tryGetNamedArg name args =
    args
    |> Seq.tryFindIndex (fun arg -> arg = name)
    |> Option.map (fun index -> index + 1)
    |> Option.bind (fun index -> Seq.tryItem index args)

printf "Install the Playwright dependencies? [y/_] "
let installingAnswer = Console.ReadKey().KeyChar
printf "\n"
if installingAnswer = 'y'
then
    printfn "Installing start."
    Microsoft.Playwright.Program.Main [|"install"|] |> ignore
    printfn "Installing end."

let main (args: string seq) =
    let html =
        tryGetNamedArg "-p" args
        |> Option.defaultValue "./index.html"
        |> File.ReadAllText

    use playwright = Playwright.CreateAsync() |> awaitTask

    let browser =
        let options = BrowserTypeLaunchOptions()
        options.Headless <- true
        playwright.Chromium.LaunchAsync options |> awaitTask

    let page = browser.NewPageAsync() |> awaitTask
    page.SetContentAsync html |> awaitUnitTask

    let pdfOptions = PagePdfOptions()
    pdfOptions.Format <- "A4"
    pdfOptions.Path <-
        tryGetNamedArg "-o" args
        |> Option.defaultValue "./convertedCV.pdf"
    page.PdfAsync pdfOptions |> awaitUnitTask

    page.CloseAsync() |> awaitUnitTask

    printfn "Converting done."

System.Environment.GetCommandLineArgs()
|> Array.toList
|> List.skip 2
|> main
