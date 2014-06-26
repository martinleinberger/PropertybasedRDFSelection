namespace PropertyBasedTypeProvider

open Microsoft.FSharp.Core.CompilerServices
open Microsoft.FSharp.Collections
open ProviderImplementation.ProvidedTypes
open System.Collections.Generic
open System.Reflection
open Wrapper
open SPARQLHttpProtocol

type Property = string

[<TypeProvider>]
type RDFTypeProvider(config : TypeProviderConfig) as this = 
    class
        inherit TypeProviderForNamespaces()
        let ns = "Uniko.West.PropertyBased"
        let asm = Assembly.GetExecutingAssembly()
        let provTy = ProvidedTypeDefinition(asm, ns, "RDFStore", Some typeof<obj>)
        let mutable conInfo : ConnectionInfo option = None
        let propertyTypeCache = new Dictionary<string, ValueType>()
        
        let makeNiceName (u:string) = 
            let fitting = 
                ConnectionManager.GetConnection(conInfo.Value).Namespaces
                |> Seq.tryFind(fun (prefix,``namespace``) -> u.StartsWith(``namespace``) )

            if fitting.IsSome
                then u.Replace((snd fitting.Value), (fst fitting.Value)+":")
                else u

        let getPropertyType (p : Property) = 
            if propertyTypeCache.ContainsKey p then propertyTypeCache.[p]
            else 
                let results = ConnectionManager.GetConnection(conInfo.Value).Query(makeProbingQuery p)
                let v = 
                    if Seq.length results = 0
                        then LITERAL
                        else
                            results
                            |> Seq.map (fun x -> x.["object"].Type)
                            |> Seq.head

                propertyTypeCache.[p] <- v
                v
        
        let convertToProperty (p : Property) = 
            match getPropertyType p with
            | URI -> 
                let s = ConnectionInfo.Serialize(conInfo.Value)
                ProvidedProperty
                    (propertyName = (makeNiceName p), propertyType = typedefof<seq<_>>.MakeGenericType(typeof<RDFResource>), 
                     GetterCode = fun args -> 
                         <@@ ((%%args.[0] : obj) :?> RDFResource).[p] |> List.map (fun x -> new RDFResource(x, s)) @@>)
            | LITERAL -> 
                ProvidedProperty
                    (propertyName = (makeNiceName p), propertyType = typedefof<seq<_>>.MakeGenericType(typeof<string>), 
                     GetterCode = fun args -> <@@ ((%%args.[0] : obj) :?> RDFResource).[p] @@>)
        
        let createUnspecifcType (properties : Property seq) = 
            let t = ProvidedTypeDefinition("Untyped", baseType = Some typeof<RDFResource>)
            let query = makeSubjectQuery properties
            let s = ConnectionInfo.Serialize(conInfo.Value)
            t.AddMember
                (ProvidedProperty
                     ("Extension", typedefof<seq<_>>.MakeGenericType(t), IsStatic = true, 
                      GetterCode = fun _ -> <@@ RDFResource.Extension(query, s) @@>))
            t
        
        let createIntersectionTypes (previouslyChosen : Property list) = 
            let s = ConnectionInfo.Serialize(conInfo.Value)
            let container = ProvidedTypeDefinition("Typed", baseType = None)
            container.AddMembersDelayed(fun _ -> 
                ConnectionManager.GetConnection(conInfo.Value).Query(makeTypeQuery previouslyChosen)
                |> Seq.map (fun x -> x.["type"].Value)
                |> Seq.map (fun classUri -> 
                       let t = ProvidedTypeDefinition((makeNiceName classUri), baseType = None)
                       let extensionQuery = makeSubjectQueryWithType previouslyChosen classUri
                       t.AddMember
                           (ProvidedProperty
                                ("Extension", typedefof<seq<_>>.MakeGenericType(t), IsStatic = true, 
                                 GetterCode = fun _ -> <@@ RDFResource.Extension(extensionQuery, s) @@>))
                       ConnectionManager.GetConnection(conInfo.Value).Query(makeClassPropertiesQuery classUri)
                       |> Seq.map (fun x -> x.["property"].Value)
                       |> Set.ofSeq
                       |> Set.union (previouslyChosen |> Set.ofList)
                       |> Seq.map convertToProperty
                       |> Seq.iter t.AddMember
                       t :> MemberInfo)
                |> Seq.toList)
            container
        
        let rec makeNestedTypes (previouslyChosen : Property list) = 
            ConnectionManager.GetConnection(conInfo.Value).Query(makePropertyQuery previouslyChosen)
            |> Seq.map (fun x -> x.["p"].Value)
            |> filter previouslyChosen
            |> Seq.map (fun property -> 
                   let x = ProvidedTypeDefinition(className = (makeNiceName property), baseType = None)
                   let updated_list = List.Cons(property, previouslyChosen)
                   x.AddMemberDelayed(fun _ -> createUnspecifcType updated_list)
                   x.AddMemberDelayed(fun _ -> createIntersectionTypes updated_list)
                   x.AddMembersDelayed(fun _ -> makeNestedTypes updated_list)
                   x :> MemberInfo)
            |> Seq.toList
        
        let buildTypes (typeName : string) (endPointUrl : string) = 
            if conInfo.IsNone then 
                conInfo <- Some { Uri = endPointUrl
                                  Prefixes = List.empty<string * string> }
            let t = ProvidedTypeDefinition(className = typeName, baseType = None)
            provTy.AddMember t
            t.AddMembersDelayed(fun _ -> makeNestedTypes [])
            t
        
        let parameters = [ ProvidedStaticParameter("endpointUrl", typeof<string>) ]
        do provTy.DefineStaticParameters(parameters, fun typeName args -> buildTypes typeName (args.[0] :?> string))
        do this.AddNamespace(ns, [ provTy ])
    end

[<TypeProviderAssembly>]
do ()
