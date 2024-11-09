#r "nuget: Markdown2Pdf, 2.2.1"

printf "\r  0%%"

let joinStrings sep (strings: string seq) = System.String.Join(sep, strings)

module Html =
    type ElMeta =
        { Name: string
          Attrs: (string * string) list }

        member this.AttrsToHtml =
            seq {
                for attr in this.Attrs ->
                    let (name, value) = attr
                    $"{name}=\"{value}\""
            }
            |> joinStrings " "

    type El =
        { Meta: ElMeta
          Content: ElContent list }

    and ElContent =
        | NextTag of Tag
        | PrimitiveContent of string

        static member toHtml(contentList: ElContent list) =
            contentList
            |> List.map (fun elContent ->
                match elContent with
                | NextTag nextTag -> nextTag.RenderEl
                | PrimitiveContent content -> content)
            |> joinStrings ""

    and Tag =
        | ClosingTag of El
        | AutoClosedTag of ElMeta

        member tag.RenderEl: string =
            match tag with
            | ClosingTag el ->
                let tagOpen =
                    $"<{el.Meta.Name}"
                    + if el.Meta.Attrs.Length <> 0 then
                          $" {el.Meta.AttrsToHtml}"
                      else
                          ""
                    + ">"

                let tagClose = $"</{el.Meta.Name}>"
                let tagContent = ElContent.toHtml el.Content
                tagOpen + tagContent + tagClose
            | AutoClosedTag el ->
                $"<{el.Name}"
                + if el.Attrs.Length <> 0 then $" {el.AttrsToHtml}" else ""
                + ">"

        member tag.Render: string =
            $"<!DOCTYPE html><html lang=\"en\"><head></head><body>{tag.RenderEl}</body></html>"

    let inline createClosed name attrs content =
        { Meta = { Name = name; Attrs = attrs }
          Content = content }
        |> ClosingTag

    let inline createAutoClosed name attrs =
        { Name = name; Attrs = attrs } |> AutoClosedTag

    let div attrs content = createClosed "div" attrs content
    let span attrs content = createClosed "span" attrs content
    let section attrs content = createClosed "section" attrs content
    let bold attrs content = createClosed "b" attrs content
    let italic attrs content = createClosed "i" attrs content
    let h1 attrs content = createClosed "h1" attrs content
    let h2 attrs content = createClosed "h2" attrs content
    let h3 attrs content = createClosed "h3" attrs content
    let h4 attrs content = createClosed "h4" attrs content
    let h5 attrs content = createClosed "h5" attrs content
    let h6 attrs content = createClosed "h6" attrs content
    let table attrs content = createClosed "table" attrs content
    let tableRow attrs content = createClosed "tr" attrs content
    let tableCol attrs content = createClosed "td" attrs content
    let tableHeadCol attrs content = createClosed "th" attrs content
    let link attrs content = createClosed "a" attrs content
    let ulist attrs content = createClosed "ul" attrs content
    let olist attrs content = createClosed "ol" attrs content
    let listItem attrs content = createClosed "li" attrs content
    let lineBreak attrs = createAutoClosed "br" attrs
    let horizontalLine attrs = createAutoClosed "hr" attrs
    let _class (value: string) = "class", value
    let _id (value: string) = "id", value
    let _href (value: string) = "href", value
    let _style (value: string) = "style", value

module Markdown =
    type MdLink = { Link: string; Text: string }

    and MdTable =
        { Headers: string list
          Rows: El list list }

    and MdListItem = { Qualifier: string; Content: El }

    and El =
        | Sequence of El list
        | Block of El list
        | Text of string
        | Bold of El
        | Italic of El
        | Header1 of string
        | Header2 of string
        | Header3 of string
        | Header4 of string
        | Header5 of string
        | Header6 of string
        | Link of MdLink
        | Table of MdTable
        | ListItem of MdListItem
        | Code of string
        | LineBreak
        | HorizontalLine
        | HtmlEmbed of Html.Tag

        member this.Render: string =
            match this with
            | Sequence sequence -> sequence |> List.map _.Render |> joinStrings ""
            | Block block ->
                let content = block |> List.map _.Render |> joinStrings ""
                $"\n\n{content}\n\n"
            | Text text -> text
            | Bold bold -> $"**{bold.Render}**"
            | Italic italic -> $"_{italic.Render}_"
            | Header1 header -> $"\n# {header}\n"
            | Header2 header -> $"\n## {header}\n"
            | Header3 header -> $"\n### {header}\n"
            | Header4 header -> $"\n#### {header}\n"
            | Header5 header -> $"\n##### {header}\n"
            | Header6 header -> $"\n###### {header}\n"
            | Link link -> $"[{link.Text}]({link.Link})"
            | Table table ->
                let headers = $"""|{joinStrings "|" table.Headers}|"""

                let tableDiv =
                    seq {
                        yield "|"
                        for _ in table.Headers -> "-|"
                    }
                    |> joinStrings ""

                let data =
                    seq {
                        for row in table.Rows ->
                            seq { for col in row -> col.Render }
                            |> joinStrings "|"
                            |> (fun builtRow -> $"|{builtRow}|")
                    }
                    |> joinStrings "\n"

                $"\n{headers}\n{tableDiv}\n{data}\n\n"
            | ListItem listItem -> $"{listItem.Qualifier} {listItem.Content.Render}\n"
            | Code code -> $"`{code}`"
            | LineBreak -> "\n\n"
            | HorizontalLine -> "\n—-\n\n"
            | HtmlEmbed tag -> tag.Render



module Document =
    type DocLink = { Text: string; Link: string }

    and DocTable =
        { Headers: string list
          Rows: DocEl list list }

    and DocList =
        | Unordered of DocEl list
        | Ordered of DocEl list

    and DocEl =
        | Sequence of DocEl list
        | RegularText of string
        | Bold of DocEl
        | Italic of DocEl
        | Header1 of string
        | Header2 of string
        | Header3 of string
        | Header4 of string
        | Header5 of string
        | Header6 of string
        | Table of DocTable
        | Link of DocLink
        | List of DocList
        | Code of string
        | LineBreak
        | HorizontalLine
        | HtmlEmbed of Html.Tag

        static member op_Implicit(str: string) = RegularText str

        static member op_Implicit(els: DocEl list) = Sequence els

        member this.ToHtml: Html.Tag =
            match this with
            | Sequence els -> Html.section [] (els |> List.map (fun el -> Html.NextTag el.ToHtml))
            | RegularText text -> Html.span [] [ Html.PrimitiveContent text ]
            | Bold el -> Html.bold [] [ Html.NextTag el.ToHtml ]
            | Italic el -> Html.italic [] [ Html.NextTag el.ToHtml ]
            | Header1 header -> Html.h1 [] [ Html.PrimitiveContent header ]
            | Header2 header -> Html.h2 [] [ Html.PrimitiveContent header ]
            | Header3 header -> Html.h3 [] [ Html.PrimitiveContent header ]
            | Header4 header -> Html.h4 [] [ Html.PrimitiveContent header ]
            | Header5 header -> Html.h5 [] [ Html.PrimitiveContent header ]
            | Header6 header -> Html.h6 [] [ Html.PrimitiveContent header ]
            | Table docTable ->
                let headers =
                    [ docTable.Headers
                      |> List.map (fun header -> Html.NextTag(Html.tableHeadCol [] [ Html.PrimitiveContent header ]))
                      |> Html.tableRow []
                      |> Html.NextTag ]

                let data =
                    docTable.Rows
                    |> List.map (fun row ->
                        row
                        |> List.map (fun col -> Html.NextTag(Html.tableCol [] [ Html.NextTag col.ToHtml ]))
                        |> Html.tableRow []
                        |> Html.NextTag)

                Html.table [] (headers @ data)
            | Link docLink -> Html.link [ Html._href docLink.Link ] [ Html.PrimitiveContent docLink.Text ]
            | List docList ->
                match docList with
                | Unordered unordered ->
                    unordered
                    |> List.map (fun item -> Html.NextTag <| Html.listItem [] [ Html.NextTag item.ToHtml ])
                    |> Html.ulist []
                | Ordered ordered ->
                    ordered
                    |> List.map (fun item -> Html.NextTag <| Html.listItem [] [ Html.NextTag item.ToHtml ])
                    |> Html.olist []
            | Code code -> Html.span [ Html._class "code" ] [ Html.PrimitiveContent code ]
            | LineBreak -> Html.lineBreak []
            | HorizontalLine -> Html.horizontalLine []
            | HtmlEmbed tag -> tag

        member this.ToMd: Markdown.El =
            match this with
            | Sequence els -> Markdown.Sequence(els |> List.map _.ToMd)
            | RegularText text -> Markdown.Text text
            | Bold bold -> Markdown.Bold bold.ToMd
            | Italic italic -> Markdown.Italic italic.ToMd
            | Header1 header -> Markdown.Header1 header
            | Header2 header -> Markdown.Header2 header
            | Header3 header -> Markdown.Header3 header
            | Header4 header -> Markdown.Header4 header
            | Header5 header -> Markdown.Header5 header
            | Header6 header -> Markdown.Header6 header
            | Table table ->
                Markdown.Table
                    { Headers = table.Headers
                      Rows = table.Rows |> List.map (fun r -> r |> List.map _.ToMd) }
            | Link link -> Markdown.Link { Text = link.Text; Link = link.Link }
            | List docList ->
                match docList with
                | Unordered unordered ->
                    unordered
                    |> List.map (fun listItem ->
                        Markdown.ListItem
                            { Qualifier = "-"
                              Content = listItem.ToMd })
                    |> Markdown.Sequence
                | Ordered ordered ->
                    ordered
                    |> List.indexed
                    |> List.map (fun (index, listItem) ->
                        Markdown.ListItem
                            { Qualifier = $"{index}."
                              Content = listItem.ToMd })
                    |> Markdown.Sequence
            | Code code -> Markdown.Code code
            | LineBreak -> Markdown.LineBreak
            | HorizontalLine -> Markdown.HorizontalLine
            | HtmlEmbed html -> Markdown.HtmlEmbed html

    let docseq = Sequence

    let table (headers, rows) =
        Table { Headers = headers; Rows = rows }

    let ol els = els |> Ordered |> List
    let ul els = els |> Unordered |> List

    let html = HtmlEmbed

    module txt =
        let r = RegularText
        let b = Bold
        let i = Italic
        let lb = LineBreak
        let hl = HorizontalLine
        let code = Code
        let a (text, link) = Link { Text = text; Link = link }

        let h =
            function
            | 1 -> Header1
            | 2 -> Header2
            | 3 -> Header3
            | 4 -> Header4
            | 5 -> Header5
            | 6 -> Header6
            | _ -> (fun t -> t |> r |> b)

        let h1 = Header1
        let h2 = Header2
        let h3 = Header3
        let h4 = Header4
        let h5 = Header5
        let h6 = Header6


open Document

let inline (!) (x: ^T) : ^U =
    ((^T or ^U): (static member op_Implicit: ^T -> ^U) x)

type DocBuilder() =
    member _.Zero = RegularText ""
    member _.Yield(str: string) = RegularText str
    member _.Yield(els: DocEl list) = Sequence els
    member _.Yield(el: DocEl) = el

    member _.Yield(els: string list) =
        els |> List.map (fun el -> RegularText el) |> Sequence

    member _.Combine(first: DocEl, second: DocEl) =
        match (first, second) with
        | Sequence seq1, Sequence seq2 -> Sequence(seq1 @ seq2)
        | Sequence dseq, el -> Sequence(dseq @ [ el ])
        | el, Sequence dseq -> Sequence(el :: dseq)
        | el1, el2 -> Sequence [ el1; el2 ]

    member _.Delay f = f ()
    member _.Run(state: DocEl) = state

let doc = DocBuilder()

let about: DocEl =
    doc {
        txt.h1 $"Marat Gevorkyan"
        "20 y.o."
        txt.lb

        [ ! "GitHub: "; txt.a ("marat0n", "https://github.com/marat0n") ]
        txt.lb

        "Primarily .NET dev, also Flutter, Python, NodeJS, Web-frontend and Rust dev."

        txt.h2 "Contact me"

        [ ! "tel: "
          txt.a ("+374 98 842 226", "tel://+37498842226")
          ! " (Armenia)"
          txt.lb ]

        [ ! "dm: telegram "; txt.a ("@m_aratka", "https://t.me/m_aratka"); txt.lb ]

        [ ! "email: "
          txt.a ("marat2255a@gmail.com", "mailto:marat2255a@gmail.com")
          txt.lb ]
    }

let smallChip text =
    html
    <| Html.span
        [ Html._style "color:2d2d2d;font-size:.8em;background:#d9d9d9;padding:1px 3px;margin:0 5px" ]
        [ Html.PrimitiveContent text ]

let workExp: DocEl =
    doc {
        txt.h2 "Work experience"

        table (
            [ "Place"; "Duration"; "My position and tasks"; "Occupation" ],
            [ [ ! "\"Blazee\" Telegram Bot"
                ! "8 months"
                ![ ! ".NET F# "
                   Code "middle"
                   ! " dev, Flutter "
                   Code "middle"
                   ! " dev, Telegram-bot creator, Web-designer, Product-manager" ]
                ! "Project-oriented" ]
              [ ! "IThub college's Ambassador"
                ! "3+ years"
                ! "Tech master-classes and courses creator and speaker (taught IT technologies to schoolers and college students)."
                ! "Project-oriented" ]
              [ ! "IThub college's IT-incubator"
                ! "2+ years"
                doc {
                    [ ! "• "; Code "junior"; ! " and "; Code "middle"; ! " .NET-dev," ]
                    html (Html.lineBreak [])
                    [ ! "• "; Code "junior"; ! " Node.js dev," ]
                    html (Html.lineBreak [])
                    [ ! "• "; Code "junior"; ! " Dart & Flutter dev," ]
                    html (Html.lineBreak [])
                    "• project-manager and dev-leader in several teams."
                }
                ! "Project-oriented" ]
              [ ! "Webcore Studio"
                ! "10 months"
                doc {
                    [ ! "Front-end "; Code "trainee"; ! " and "; Code "junior"; ! " developer." ]
                    html (Html.lineBreak [])
                    "Stack: pure HTML/CSS/JS + Gulp."
                }
                ! "Fulltime" ] ]
        )

        txt.h3 "Open source"

        ul
            [ ![ txt.a ("DragulaDropula", "https://github.com/marat0n/DragulaDropula")
                 ! " "
                 Code "2.1.2"
                 txt.r " — A .NET Blazor package for dragging-and-droppping functionality (for canvas-oriented UX)." ]
              ![ txt.a ("KarteVonMorgen", "https://github.com/marat0n/kartevonmorgen.ts")
                 txt.r
                     " — \"The map of tomorrow\", a map accumulated all good places, where I participated in updating the landing-page and map-functionality in the russian version of project, before it was cancelled because of the "
                 txt.a ("war", "https://en.wikipedia.org/wiki/Russian_invasion_of_Ukraine")
                 txt.r "." ]
              ![ txt.a ("Telefunc", "https://github.com/marat0n/telefunc")
                 ! " "
                 Code "alpha"
                 ! " — A F# package for pretty and functional handling your Telegram-bot events." ]
              ![ txt.a ("yerevan.rs", "https://github.com/marat0n/yerevan.rs")
                 ! " "
                 Code "0.1.4"
                 ! " — A Rust crate implemented the "
                 txt.a (
                     "Copmutation Expressions",
                     "https://fsharpforfunandprofit.com/posts/computation-expressions-intro/"
                 )
                 ! " idea (or "
                 txt.a ("do-notations", "https://en.wikibooks.org/wiki/Haskell/do_notation")
                 ! ") in the Rust language." ] ]
    }

let skills =
    doc {
        txt.h2 "Skills"
        [ txt.b <| ! "Languages"; ! ": Russian native, English B2, Armenian A1." ]

        txt.h3 "Programming"

        table (
            [ "Runtime"; "PLs"; "Additional info" ],
            [ [ ! ".NET 5-8"
                ! "C#, F#"
                ! "EF Core, ADO.NET, Blazor, ASP.NET, Minimal API, ASP MVC, Giraffe, Elmish" ]
              [ ! ".NET Framework"; ! "C#"; ! "Win Forms" ]
              [ ! "Flutter"; ! "Dart, Python, F#"; ! "Dio, BLoC, MVU" ]
              [ ! "Node"; ! "JS, TS, Elm"; ! "Rust's compiled WASM, Express, React, Angular" ]
              [ ! "Python"; ! "Python"; ! "Django, Flask, FastAPI, NumPy, Google Collab" ]
              [ ! "C/C++"; ! "C/C++"; ! "no libs, std only" ]
              [ ! "Rust"; ! "Rust"; ! "Axum, Warp, Yew, Serde, Tokio, Libsql" ]
              [ ! "Browser"
                ! "HTML/CSS, JS, Elm"
                ! "Bootstrap, Elm, pure HTML/CSS/JS web-app" ] ]
        )

        txt.lb
        "Also: PHP, Ruby, Lua, Arduino, Godot, Unity."

        txt.h3 "Database managment systems"

        ul
            [ ! "MariaDB / MySQL"
              ! "PostreSQL"
              ! "MS SQL"
              ! "Airtable"
              ! "SQLite"
              ! "Turso (SQLite with libsql)"
              ![ txt.i <| ! "Can help you migrate from Oracle"; ! " ;）" ] ]

        txt.h3 "Tools (just my tools)"
        "Windows 11, Ubuntu 22.04, Arch, NeoVim, Vi/Vim keymaps, nushell, tmux, JetBrains Rider & PyCharm, Zed, curlx, Figma, git, GitHub, GitLab, Trello, Notion, MS Planner, Wix, Azure, Netlify, Heroku, Google Cloud, Docker, Nginx."
    }

let education =
    doc {
        txt.h2 "Education"

        table (
            [ "Where"; "Time"; "Place"; "Additional info" ],
            [ [ ! "School"; ! "9 y."; ! "Korolyov, Balashicha, Moscow"; ! "" ]
              [ ! "IThub College"
                ! "4 y."
                ! "Moscow"
                ! "Field: Information Technology and Programming. Did volunteering, hackathons and programming master-classes for other studients." ]
              [ ! "Self-education"
                ! "Now"
                ! ""
                ! "Learning new skills or getting better at: Golang, Tact and TVM, Data-oriented programming and CS." ] ]
        )
    }

let conditions =
    doc {
        txt.h2 "Conditions"
        txt.b <| ! "Salary expectations:"
        txt.lb

        ul
            [ ! "For project-oriented work: from 20$ per hour;"
              ! "For fulltime work: from 2000$ per month." ]

        txt.lb

        [ txt.b <| ! "My location"
          ! ": Yerevan, can do work online or in local office." ]
    }

let cvdotnet =
    doc {
        about
        workExp
        education
        skills
        conditions
    }


printf "\r 10%%"
System.IO.File.WriteAllText("marat_gevorkyan_cv.html", cvdotnet.ToHtml.Render)
printf "\r 40%%"
System.IO.File.WriteAllText("marat_gevorkyan_cv.md", cvdotnet.ToMd.Render)
printf "\r 70%%"

let options = Markdown2Pdf.Options.Markdown2PdfOptions()
options.KeepHtml <- true
options.HeaderHtml <- ""

options.FooterHtml <-
    $"<i style='font-size:10pt;margin:0 0 20px 20px'>Marat Gevorkyan's CV. Built in {System.DateOnly.FromDateTime System.DateTime.Now}.</i>"

options.MarginOptions <- new Markdown2Pdf.Options.MarginOptions()
options.MarginOptions.Top <- "80px"
options.MarginOptions.Bottom <- "50px"
options.MarginOptions.Left <- "40px"
options.MarginOptions.Right <- "40px"
printf "\r 71%%"
let converter = new Markdown2Pdf.Markdown2PdfConverter(options)
printf "\r 72%%"

converter.Convert [ "marat_gevorkyan_cv.md" ]
|> Async.AwaitTask
|> Async.RunSynchronously

printf "\rDone\n"
