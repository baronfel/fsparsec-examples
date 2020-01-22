#r "/Users/chethusk/.nuget/packages/fparsec/1.1.0/lib/netstandard2.0/FParsecCS.dll"
#r "/Users/chethusk/.nuget/packages/fparsec/1.1.0/lib/netstandard2.0/FParsec.dll"

open FParsec.CharParsers
open FParsec

(* Spec is https://www.w3.org/Protocols/rfc2616/rfc2616-sec5.html *)

type RequestLine = string
type Headers = string list
type Body = string

type Request = 
  { request: RequestLine 
    headers: Headers
    body: Body option }
    static member empty = 
      { request = ""
        headers = [] 
        body = None }
    static member create line headers body =
      { request = line
        headers = headers
        body = body }

type State = unit

let anyCharNotNewline = satisfy (fun c -> c <> '\r' && c <> '\n')

let pLine = many1CharsTill anyCharNotNewline newline <?> "HTTP Header"
let pHeader = many1CharsTill anyCharNotNewline newline <?> "HTTP Header Value"
let pHeaders = many pHeader <?> "HTTP Headers"
let pBody = opt (many1CharsTill anyCharNotNewline newline) <?> "HTTP Body Content"

let pRequest: Parser<Request, State> =
  pipe3 pLine
        pHeaders
        pBody
        Request.create

let parse = 
    runParserOnString pRequest () "httprequest" >> function | Success(req,_,_) -> Result.Ok req | Failure(_, err, _) -> Result.Error err

let tests =
    ["""GET /pub/WWW/TheProject.html HTTP/1.1

""", Request.create "GET /pub/WWW/TheProject.html HTTP/1.1" [] None]

let runTests () = 
  for sample, expected in tests do 
    match parse sample with
    | Result.Ok actual when actual = expected -> () 
    | Result.Ok actual -> failwithf "Actual <> Expected.\nGot\n%A\nbut wanted\n%A" actual expected
    | Result.Error err -> failwithf "Error parsing \"%s\":\n%A" sample err
