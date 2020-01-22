#r "/Users/chethusk/.nuget/packages/fparsec/1.1.0/lib/netstandard2.0/FParsecCS.dll"
#r "/Users/chethusk/.nuget/packages/fparsec/1.1.0/lib/netstandard2.0/FParsec.dll"

open FParsec.CharParsers
open FParsec

#load "./spec.fsx"
open Spec

(* Spec is https://www.w3.org/Protocols/rfc2616/rfc2616-sec5.html *)

type Method = 
| Options
| Get
| Head
| Post
| Put
| Delete
| Trace
| Connect
| Custom of method: string

type HttpVersion = 
  { major: uint32
    minor: uint32 }
    static member create maj min = 
      { major = maj
        minor = min }

type RequestLine = 
  { method: Method
    uri: Uri.Uri
    version: HttpVersion }
  static member create method uri version = 
    { method = method
      uri = uri
      version = version }

type Headers = string list
type Body = string

type Request = 
  { request: RequestLine 
    headers: Headers
    body: Body option }
    static member create line headers body =
      { request = line
        headers = headers
        body = body }

type State = unit

let anyCharNotNewline = satisfy (fun c -> c <> '\r' && c <> '\n')

let a b f = b .>> f

let pMethod = 
  choice [
    stringReturn "OPTIONS" Options
    stringReturn "GET" Get
    stringReturn "HEAD" Head
    stringReturn "POST" Post
    stringReturn "PUT" Put
    stringReturn "DELETE" Delete
    stringReturn "TRACE" Trace
    stringReturn "CONNECT" Connect
    pToken |>> Custom
  ] <?> "HTTP Method"

let pHttpVersion = 
  pipe4 (pstringCI "HTTP/") puint32 (pchar '.') puint32 (fun _ maj _ min -> HttpVersion.create maj min) <?> "HTTP Version"

let pLine = 
  pipe3 (pMethod .>> pSpace)
        (Uri.uri .>> pSpace)
        pHttpVersion 
        RequestLine.create .>> newline <?> "HTTP Request Line"
let pHeader = many1CharsTill anyCharNotNewline newline <?> "HTTP Header Value"
let pHeaders = many pHeader <?> "HTTP Headers"
let pBody = opt (many1CharsTill anyCharNotNewline newline) <?> "HTTP Body Content"

let pRequest: Parser<Request, State> =
  pipe3 pLine
        pHeaders
        pBody
        Request.create

let parse = 
    runParserOnString pRequest () "httprequest" >> function | Success(req,_,_) -> Result.Ok req | Failure(errmsg, _, _) -> Result.Error errmsg

let tests =
    ["""GET /pub/WWW/TheProject.html dfahjklHTTP/1.1

""", Request.create { method = Get; uri = Uri.AbsolutePath "/pub/WWW/TheProject.html"; version = HttpVersion.create 1u 1u } [] None]

let runTests () = 
  for sample, expected in tests do 
    match parse sample with
    | Result.Ok actual when actual = expected -> () 
    | Result.Ok actual -> failwithf "Actual <> Expected.\nGot\n%A\nbut wanted\n%A" actual expected
    | Result.Error err -> failwithf "Error parsing \"%s\":\n%s" sample err
