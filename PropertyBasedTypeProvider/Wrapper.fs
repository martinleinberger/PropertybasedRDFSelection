module Wrapper

open SPARQLHttpProtocol
open System
open System.Text.RegularExpressions
open System.Collections.Generic



type ConnectionInfo = 
    { Uri : string
      Prefixes : (string * string) list }
    
    static member Serialize(x : ConnectionInfo) = 
        x.Uri + "$;" + (x.Prefixes
                       |> List.map (fun (i, j) -> i + "$=" + j)
                       |> String.concat "$,")
    
    static member Deserialize(x : string) : ConnectionInfo = 
        let uri = Regex.Split(x,"\$;").[0]
        let prefixes =
            let y = Regex.Split(x,"\$;").[1]
            if y = "" 
                then List.empty<string*string>
                else
                    Regex.Split(y,"\$,")
                    |> Array.map(fun x -> Regex.Split(y,"$=").[0],Regex.Split(y,"$=").[1] )
                    |> Array.toList
                       
        { Uri = uri
          Prefixes = prefixes }

type ConnectionManager() = 
    class
        static let connections = new Dictionary<string, SPARQLHttpEndpoint>()
        static member GetConnection (serializedConInfo:string) = 
            if not (connections.ContainsKey serializedConInfo) then
                let conInfo = ConnectionInfo.Deserialize(serializedConInfo)
                let con = new SPARQLHttpEndpoint(conInfo.Uri)
                let sx = con.Namespaces
                //conInfo.Prefixes |> Seq.iter con.AddPrefix
                connections.Add (serializedConInfo,con)

            connections.[serializedConInfo]
        static member GetConnection (conInfo:ConnectionInfo) = 
            ConnectionManager.GetConnection(ConnectionInfo.Serialize(conInfo))
    end

[<StructuredFormatDisplay("{InstanceUri}")>]
type RDFResource(instanceUri : string, serializedConinfo : string) = 
    class
        static let getCon (url:string) = ConnectionManager.GetConnection url
        
        static member Extension(query : string, serializedConinfo : string) : RDFResource seq = 
            let rec f (query : string) (limit : int) (offset : int) = 
                seq { 
                    let query' = query + "\nLIMIT " + (string limit) + "\nOFFSET " + (string offset)
                    let results = getCon(serializedConinfo).Query query' |> Seq.toList
                    for x in results do
                        yield RDFResource(x.["s"].Value, serializedConinfo)
                    if not (results.Length < limit) then
                        yield! f query limit (offset + limit)
                }
            f query 1000 0
        
        member __.InstanceUri : string = instanceUri
        member __.Type : string list = __.["http://www.w3.org/1999/02/22-rdf-syntax-ns#type"]
        member __.Item  
            with get(propertyUri) : string list = 
                let instanceUri', propertyUri' = "<" + instanceUri + ">", "<" + propertyUri + ">"
                let query = "SELECT ?value WHERE { " + instanceUri' + " " + propertyUri' + " ?value . }"
                getCon(serializedConinfo).Query(query)
                |> Seq.map (fun x -> x.["value"].Value)
                |> Seq.toList
                              
    end

let transformPattern (properties : string seq) = 
    properties
    |> Seq.mapi (fun index url -> String.Format("?s <{0}> {1} . ", url, "?o" + string (index)))
    |> String.concat "\n"

let filter (previouslyChosen : string list) (results : string seq) = 
    let previouslyChosen' = "http://www.w3.org/1999/02/22-rdf-syntax-ns#type" :: previouslyChosen
    let contains l x = l |> List.exists ((=) x)
    results |> Seq.filter (fun x -> not (contains previouslyChosen' x))

let makeSubjectQuery (properties : string seq) = 
    let pattern = transformPattern properties
    "SELECT DISTINCT ?s WHERE { " + pattern + " }"

let makeSubjectQueryWithType (properties :string seq) (rdfType:string) = 
    let pattern = transformPattern properties
    "SELECT DISTINCT ?s WHERE { ?s a <" + rdfType + "> . " + pattern + " }" 

let makePropertyQuery (properties : string seq) = 
    let pattern = transformPattern properties
    "SELECT DISTINCT ?p WHERE { ?s ?p ?o . " + pattern + " }"

let makeTypeQuery (properties : string seq) = 
    let pattern = transformPattern properties
    "SELECT DISTINCT ?type WHERE { ?s a ?type . " + pattern + " }"

let makeClassPropertiesQuery (classUri : string) = 
    "SELECT ?property WHERE { ?property <http://www.w3.org/2000/01/rdf-schema#domain> <"+classUri+"> . }"

let makeProbingQuery (propertyUri : string) = 
    "SELECT ?object WHERE { ?subject <" + propertyUri + "> ?object . } LIMIT 1"



