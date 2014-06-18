namespace PropertyBasedTypeProvider

open System
open SPARQLHttpProtocol

type Property = string

type Store(endpointUrl) = 
    class
        let connection = new SPARQLHttpEndpoint(endpointUrl)
        
        do 
            [ "rdf", "http://www.w3.org/1999/02/22-rdf-syntax-ns#"
              "rdfs", "http://www.w3.org/2000/01/rdf-schema#" ]
            |> Seq.iter connection.AddPrefix
        
        member this.Connection = connection
        member this.EndpointUrl = endpointUrl
        member this.Properties(previouslyChosen : Property list) : Property seq = 
            let pattern = 
                if previouslyChosen = [] then ""
                else 
                    (previouslyChosen
                     |> Seq.mapi (fun index x -> String.Format("?x <{0}> {1} . ", x, "?y" + string (index)))
                     |> String.concat " ")
            
            let contains l x = l |> List.exists ((=) x)
            let query = "SELECT DISTINCT ?p WHERE { ?x ?p ?y ." + pattern + "}"
            //System.Diagnostics.Debug.WriteLine query
            connection.Query(query)
            |> Seq.map (fun x -> x.["p"].Value)
            |> Seq.filter (fun x -> not (contains previouslyChosen x))
    end