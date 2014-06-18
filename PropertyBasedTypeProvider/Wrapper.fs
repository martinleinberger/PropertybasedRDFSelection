module Wrapper

open SPARQLHttpProtocol
open System
open System.Text.RegularExpressions
open System.Collections.Generic

type ConnectionManager() = 
    class
        static let connections = new Dictionary<string, SPARQLHttpEndpoint>()
        static member AddConnection url connection = connections.[url] <- connection
        static member GetConnection url = connections.[url]
        static member Contains url = connections.ContainsKey url
    end

type ConnectionInfo = 
    { Uri : string
      Prefixes : (string * string) list }
    
    static member Serialize(x : ConnectionInfo) = 
        x.Uri + "$;" + (x.Prefixes
                       |> List.map (fun (i, j) -> i + "$=" + j)
                       |> String.concat "$,")
    
    static member Deserialize(x : string) : ConnectionInfo = 
        let uri = Regex.Split("\$;",x).[0]
        let prefixes =
            let y = Regex.Split("\$;",x).[1]
            if y = "" 
                then List.empty<string*string>
                else
                    Regex.Split("\$,",y)
                    |> Array.map(fun x -> Regex.Split("$=", x).[0],Regex.Split("$=", x).[1] )
                    |> Array.toList
                       
        { Uri = uri
          Prefixes = prefixes }

[<StructuredFormatDisplay("{InstanceUri}")>]
type RDFResource(instanceUri : string, endpoint : string) = 
    class
        static let getCon url = ConnectionManager.GetConnection url
        
        static member Extension(query : string, ?endpointUrl' : string) : RDFResource seq = 
            let url = 
                defaultArg endpointUrl' "http://stardog.west.uni-koblenz.de:8080/openrdf-sesame/repositories/Jamendo/"
            
            let rec f (query : string) (limit : int) (offset : int) = 
                seq { 
                    let query' = query + "\nLIMIT " + (string limit) + "\nOFFSET " + (string offset)
                    printfn "%A" query'
                    let results = getCon(url).Query "" |> Seq.toList
                    for x in results do
                        yield RDFResource(x.["s"].Value, url)
                    if not (results.Length < limit) then yield! f query limit (offset + limit)
                }
            f query 1000 0
        
        member __.InstanceUri : string = instanceUri
        member __.Type : string list = []
        member __.Item  
            with get(propertyUri) : string list = 
                let instanceUri', propertyUri' = "<" + instanceUri + ">", "<" + propertyUri + ">"
                let query = "SELECT ?value WHERE { " + instanceUri' + " " + propertyUri' + " ?value . }"
                printfn "%A" query
                getCon(endpoint).Query("")
                |> Seq.map (fun x -> x.["value"].Value)
                |> Seq.toList //let findInstances (query:string) : RDFResource seq=  
                              
    end

let transformPattern (properties : string seq) = 
    properties
    |> Seq.mapi (fun index url -> String.Format("?s <{0}> {1} . ", url, "?o" + string (index)))
    |> String.concat "\n"

let filter (previouslyChosen : string list) (results : string seq) = 
    let previouslyChosen' = "http://www.w3.org/1999/02/22-rdf-syntax-ns#type" :: previouslyChosen
    let contains l x = l |> List.exists ((=) x)
    results |> Seq.filter (fun x -> not (contains previouslyChosen x))

let makeSubjectQuery (properties : string seq) = 
    let pattern = transformPattern properties
    "SELECT DISTINCT ?s WHERE { " + pattern + " }"

let makePropertyQuery (properties : string seq) = 
    let pattern = transformPattern properties
    "SELECT DISTINCT ?p WHERE { ?s ?p ?o . " + pattern + " }"
