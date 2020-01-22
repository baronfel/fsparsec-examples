#r "/Users/chethusk/.nuget/packages/fparsec/1.1.0/lib/netstandard2.0/FParsecCS.dll"
#r "/Users/chethusk/.nuget/packages/fparsec/1.1.0/lib/netstandard2.0/FParsec.dll"

open FParsec.CharParsers
open FParsec

(* parsers for basic HTTP tokens *)


let charBetween min max label = 
    satisfyL (fun c -> 
        let b = byte c 
        min <= b && b <= max) label

let inline singleChar byteVal label = satisfyL (byte >> (=) byteVal) label

(* primitives *)
let upperChar<'state> : Parser<char, 'state> = charBetween (byte 'A') (byte 'Z') "uppercase character"

let lowerChar<'state> : Parser<char, 'state> = charBetween (byte 'a') (byte 'z') "lowercase character"

let alphaChar<'state> : Parser<char, 'state> = upperChar <|> lowerChar

let pCR<'state> : Parser<char, 'state> = singleChar 13uy "CR"

let pLF<'state> : Parser<char, 'state> = singleChar 10uy "LF"

let pSpace<'state> : Parser<char, 'state> = singleChar 32uy "space"

let pHorizontalTab<'state> : Parser<char, 'state> = singleChar 9uy "horizontal tab"

let pDoubleQuote<'state> : Parser<char, 'state> = singleChar 34uy "double-quote"

let pAscii<'state> : Parser<char, 'state> = charBetween 0uy 127uy "ASCII char"

let pControl<'state> : Parser<char, 'state> = 
    charBetween 0uy 31uy "control character" <|> singleChar 127uy "DEL character"

let pCLRF<'state> : Parser<unit, 'state> = pCR >>. pLF |>> ignore

(* higher-level concepts *)

let pSeparator<'state> : Parser<char, 'state> = 
    let seps = "()<>@,;:\\\"/[]?={}"
    choiceL [
        anyOf seps
        pSpace
        pHorizontalTab
    ] "separator character"

let pToken<'state> : Parser<string, 'state> =
    manyCharsTill (satisfy System.Char.IsLetterOrDigit) pSeparator <?> "token"

(* where we'll put parsing for spec-compliant uris: https://www.ietf.org/rfc/rfc2396.txt *)
module Uri = 

    let digit<'state> : Parser<char[], 'state> = anyOf "0123456789" |>> Array.singleton <?> "digit"
    let upalpha<'state> : Parser<char[], 'state> = anyOf ['A'..'Z'] |>> Array.singleton <?> "uppercase"
    let lowalpha<'state> : Parser<char[], 'state> = anyOf ['a'..'z'] |>> Array.singleton <?> "lowercase"
    let alpha<'state> : Parser<char[], 'state> = (lowalpha <|> upalpha) <?> "alpha"
    let alphanum<'state> : Parser<char[], 'state> = (alpha <|> digit) <?> "alphanum"
    let pHex<'state> : Parser<char[], 'state> = digit <|> (anyOf "ABCDEFabcdef" <?> "hex character" |>> Array.singleton) <?> "single hex character"
    let escaped<'state> : Parser<char[], 'state> = (pchar '%' >>. (pHex .>>. pHex) |>> fun (h1, h2) -> Array.append h1 h2) <?> "escaped hex value"
    let mark<'state> : Parser<char[], 'state> = (anyOf "-_.!~*\'()" |>> Array.singleton) <?> "mark"
    let unreserved<'state> : Parser<char[], 'state> = (alphanum <|> mark) <?> "unreserved"
    let reserved<'state> : Parser<char[], 'state> = (anyOf ";/?:@&=+$," |>> Array.singleton) <?> "reserved"
    let uric<'state> : Parser<char[], 'state> = (reserved <|> unreserved <|> escaped) <?> "uric"
    
    let fragment<'state> : Parser<char[], 'state> = (many uric |>> Array.concat) <?> "fragment"
    let query<'state> : Parser<char[], 'state> = (many uric |>> Array.concat) <?> "query"

    let urichar<'state> : Parser<char[], 'state> = unreserved <|> escaped <|> (anyOf ":@&=+$," |>> Array.singleton) <?> "urichar"
    let segment<'state> : Parser<char[], 'state> = many urichar  |>> Array.concat <?> "segment"
    let param<'state> : Parser<char[], 'state> = sepBy segment (pstring ";") |>> Array.concat <?> "param"
    let pathSegments<'state> : Parser<char[], 'state> =
        sepBy1 segment (pstring "/") <?> "segments"
        |>> fun segments -> 
            segments 
            |> List.map (fun seg -> Array.append seg [| '/' |]) 
            |> Array.concat 
            |> System.String
            |> fun s -> s.TrimEnd('/') |> Array.ofSeq
        

    let inline digitsToInt converter = 
        many digit |>> (Array.concat >> System.String >> converter)
    
    let port<'state> : Parser<uint16, 'state> = digitsToInt uint16 <?> "port"

    let ipv4Address<'state> : Parser<uint8*uint8*uint8*uint8, 'state> = 
        pipe4 (digitsToInt uint8 .>> pstring ".")
              (digitsToInt uint8 .>> pstring ".")
              (digitsToInt uint8 .>> pstring ".")
              (digitsToInt uint8)
              (fun a b c d -> a, b, c, d) <?> "ipv4"

    let toplabel<'state> : Parser<char[], 'state> = 
        alpha 
        <|> (pipe3 alpha 
                   (many (alphanum <|> (pchar '-' |>> Array.singleton)))
                   alphanum 
                   (fun start middle fin -> 
                    Array.concat [|
                        yield start
                        yield! middle
                        yield fin
                    |]
                   )
               ) <?> "toplabel"
    
    let domainlabel<'state> : Parser<char[], 'state> =
        alphanum 
        <|> pipe3 alphanum
                  (many (alphanum <|> (pchar '-' |>> Array.singleton)))
                  alphanum
                  (fun start middle fin -> Array.concat [|
                        yield start
                        yield! middle
                        yield fin
                    |]) <?> "domain label"
                    
    let hostname<'state> : Parser<char[], 'state> =
        pipe3 (sepBy domainlabel (pstring "."))
              (pchar '.' >>. toplabel)
              (opt (pchar '.' |>> Array.singleton))
              (fun domains top dot ->
                Array.concat [ yield! domains; yield top; yield! Option.toArray dot]
              ) <?> "hostname"

    type Host = 
    | Hostname of char[] 
    | Ip of System.Net.IPAddress
      member x.Chars = 
        match x with
        | Hostname chars -> chars
        | Ip ip -> ip |> string |> Array.ofSeq

    let host<'state> : Parser<Host, 'state> = hostname |>> Hostname <|> (ipv4Address |>> fun (a,b,c,d) -> System.Net.IPAddress([|byte a; byte b; byte c; byte d |]) |> Ip) <?> "host"

    let hostport<'state> : Parser<Host * uint16 option, 'state> = host .>>. opt (pstring ":" >>. port) <?> "hostport"

    let userinfo<'state> : Parser<char[], 'state> = many (unreserved <|> escaped <|> (anyOf ";:&=+$," |>> Array.singleton)) |>> Array.concat <?>"userinfo"
    let server<'state> : Parser<char [] option * Host * uint16 option, 'state> = 
        pipe2 (opt (userinfo .>> pstring "@")) hostport (fun u (h, p) -> u, h, p) <?> "server"
    let regname<'state> : Parser<char[], 'state> = 
        many1 (choice [
                    unreserved
                    escaped
                    anyOf "$,;:@&=+" |>> Array.singleton
                ]) |>> Array.concat <?> "regname"
    
    type Authority =
    | Server of user: char [] option * host: Host * port: uint16 option
    | RegName of char[]
        member x.Chars = 
            match x with 
            | Server(user, host, port) -> 
                Array.concat [ yield! Option.toArray user
                               yield host.Chars
                               yield! Option.toArray (port |> Option.map (string >> Array.ofSeq)) ]
            | RegName chars -> chars
    
    let authority<'state> : Parser<Authority, 'state> = (server |>> Server) <|> (regname |>> RegName) <?> "authority"

    let scheme<'state> : Parser<char[], 'state> = pipe2 alpha (many (alpha <|> digit <|> (anyOf "+-." |>> Array.singleton))) (fun s rest -> Array.concat [ s; yield! rest ]) <?> "scheme"
    let relsegment<'state> : Parser<char[], 'state> = many1 (unreserved <|> escaped <|> (anyOf ";@&=+$," |>> Array.singleton)) |>> Array.concat <?> "relsegment"
    let abspath<'state> : Parser<char[], 'state> = pipe2 (pchar '/') pathSegments (fun slash paths -> Array.append [|'/'|] paths) <?> "abspath"
    let relpath<'state> : Parser<char[], 'state> = 
        pipe2 relsegment 
              (opt abspath) 
              (fun rel abs -> 
                    match abs with 
                    | Some abs -> Array.concat [ [|'/'|]; rel; abs ]
                    | None -> rel
                ) <?> "relpath"
    let netpath<'state> : Parser<Authority * char [] option, 'state> = pstring "//" >>. authority .>>. opt abspath <?> "netpath"
    let uric_no_slash<'state> : Parser<char[], 'state> = unreserved <|> escaped <|> (anyOf ";?:@&=+$," |>> Array.singleton) <?> "uric_no_slash"
    let opaquepart<'state> : Parser<char[], 'state> = pipe2 uric_no_slash (many uric) (fun u us -> Array.concat [u; yield! us]) <?> "opaquepart"
    
    type Heir = 
    | Authority of Authority * absPath: char [] option
    | AbsPath of char []
        member x.Chars =
            match x with
            | Authority(a, Some abs) ->
                Array.append a.Chars abs
            | Authority(a, None) -> a.Chars
            | AbsPath a -> a

    let heirpart<'state> : Parser<Heir * char [] option, 'state> = 
        (netpath |>> Heir.Authority) <|> (abspath |>> Heir.AbsPath)
        .>>. opt (pstring "?" >>. query) <?> "heirpaart"
    
    let absoluteUri<'state> : Parser<char[], 'state> =
        let heirToChars = 
            heirpart |>> fun (heir, query) ->
                match query with
                | Some q -> 
                    Array.append heir.Chars [| yield '?'; yield! q |]
                | None -> heir.Chars

        pipe2 (scheme .>> pchar ':')
              (heirToChars <|> opaquepart)
              (fun scheme other -> Array.concat [scheme; [| ':' |]; other ]) <?> "absolute uri"

    type Uri = 
    | Any
    | Absolute of string
    | AbsolutePath of string
    | Authority of string

    let uri<'state> : Parser<Uri, 'state> = 
        choice [
            charReturn '*' Any <?> "any"
            abspath |>> (System.String >> AbsolutePath)
            absoluteUri |>> (System.String >> Absolute)
            authority |>> fun a -> a.Chars |> System.String |> Authority
        ]