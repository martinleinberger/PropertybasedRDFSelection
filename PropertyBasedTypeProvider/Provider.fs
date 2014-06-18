namespace PropertyBasedTypeProvider

open Microsoft.FSharp.Core.CompilerServices
open Microsoft.FSharp.Collections
open ProviderImplementation.ProvidedTypes
open System.Reflection
open Wrapper
open SPARQLHttpProtocol

//        let mutable connection : Store option = None
//        let mutable endpointUrl' = ""
[<TypeProvider>]
type RDFTypeProvider(config : TypeProviderConfig) as this = 
    class
        inherit TypeProviderForNamespaces()
        let ns = "Uniko.West.PropertyBased"
        let asm = Assembly.GetExecutingAssembly()
        let provTy = ProvidedTypeDefinition(asm, ns, "RDFStore", Some typeof<obj>)
        let mutable url : string option = None
        let convertToProperty (p : Property) = 
            ProvidedProperty
                (propertyName = p, propertyType = typedefof<seq<_>>.MakeGenericType(typeof<string>), 
                 GetterCode = fun args -> <@@ ((%%args.[0] : obj) :?> RDFResource).[p] @@>)
        
        let createUnspecifcType (properties : Property seq) = 
            let t = ProvidedTypeDefinition("Untyped", baseType = Some typeof<RDFResource>)
            let query = makeSubjectQuery properties
            let s = url.Value
            t.AddMember
                (ProvidedProperty
                     ("Extension", typedefof<seq<_>>.MakeGenericType(t), IsStatic = true, 
                      GetterCode = fun _ -> <@@ RDFResource.Extension(query, s) @@>))
            t
        
//        let makeInstantiableTypes (properties : Property seq) = 
//            let typed = ProvidedTypeDefinition(className = "Typed", baseType = Some typeof<obj>)
//            let query = Wrapper.makeSubjectQuery properties
//            let s = url.Value
//            typed.AddMember
//                (ProvidedProperty
//                     (propertyName = "Extension", propertyType = typedefof<seq<_>>.MakeGenericType(typed), 
//                      IsStatic = true, GetterCode = fun _ -> <@@ RDFResource.Extension(query, s) @@>))
//            let ctor = 
//                ProvidedConstructor(parameters = [ new ProvidedParameter("instanceUri", typeof<string>) ], 
//                                    InvokeCode = fun args -> 
//                                        <@@ let instanceUri = (%%args.[0] : string)
//                                            new RDFResource(instanceUri, s) @@>)
//            
//            let properties' = 
//                properties
//                |> Seq.map convertToProperty
//                |> Seq.toList
//            
//            typed.AddMember ctor
//            typed.AddMembers properties'
//            [ typed ]
//        
        let rec makeNestedTypes (previouslyChosen : string list) = 
            ConnectionManager.GetConnection(url.Value).Query(makePropertyQuery previouslyChosen)
            |> Seq.map (fun x -> x.["p"].Value)
            |> filter previouslyChosen
            |> Seq.map (fun property -> 
                   let x = ProvidedTypeDefinition(className = property, baseType = None)
//                   x.AddMembers(makeInstiableTypes (property :: previouslyChosen))
                   x.AddMemberDelayed(fun _ -> createUnspecifcType (List.Cons(property, previouslyChosen)))
//                   x.AddMembersDelayed(fun _ -> makeNestedTypes (List.Cons(property, previouslyChosen)))
                   x :> MemberInfo)
            |> Seq.toList
        
        //connection.Value.Properties previouslyChosen
        let buildTypes (typeName : string) (endPointUrl : string) = 
            if not (ConnectionManager.Contains endPointUrl) then 
                ConnectionManager.AddConnection endPointUrl (new SPARQLHttpEndpoint(endPointUrl))
                url <- Some endPointUrl
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
