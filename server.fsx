#r "nuget: Akka.FSharp" 
#r "nuget: Akka.TestKit"
#r "nuget: Akka.Remote"
#r "nuget: Akka.Serialization.Hyperion"
#time "on"
open System
open Akka.FSharp
open Akka.Remote
open Akka.Configuration
open Akka.Serialization
open Akka.Actor
open System.Security.Cryptography; 
let byteToHex : byte -> string =
    fun b -> b.ToString("x2")

let SHA_256 (phase : string) = 
    let bytePhase=System.Text.Encoding.ASCII.GetBytes(phase)
    SHA256.Create().ComputeHash(bytePhase)
    |>fun bytes -> bytes |> Array.fold (fun a x -> a + (byteToHex x)) ""

let check0(k:int)(phase:string)=
        let mutable array=""
        for i=0 to k do
            array<-array.Insert(i,"0")
        let newArray=array.Substring(0,k)
        //printf "%s"newArray
        if(phase.StartsWith(newArray)) then true
        else false       
      
let find256 (prefix:string) (n:int) (k:int) (count:int)= //n:prefix后面的个数,k:前面有几个0;count:要多少值
      let mutable newCount= count
     // let mutable final256= ""
      while  (not(n.Equals(0))&&newCount>0) do
                let mutable phase=prefix
                for i = 1 to n do
                    phase <- phase+ char(Random().Next(32,127)).ToString()
                let sb256=SHA_256(phase)  
           // printfn "sssss:%i"phase
                if check0 k sb256 then 
                    printfn $"{phase} {phase.Length} {sb256}"
                   // final256<-sb256
                    newCount<-newCount-1
     // final256
let configuration = 
    ConfigurationFactory.ParseString(
        @"akka {
            actor {
                provider = ""Akka.Remote.RemoteActorRefProvider, Akka.Remote""
                debug : {
                    receive : on
                    autoreceive : on
                    lifecycle : on
                    event-stream : on
                    unhandled : on
                }
                serializers {
                    hyperion = ""Akka.Serialization.HyperionSerializer, Akka.Serialization.Hyperion""
                }
                serialization-bindings {
                    ""System.Object"" = hyperion
                }                
            }
            remote {
                helios.tcp {
                    port = 9002
                    hostname = 10.136.32.178
                }
            }
        }")

let system = ActorSystem.Create("RemoteFSharp", configuration)
let server = 
    spawn system "server"
        <|fun mailbox ->
            let rec loop() = actor {
                let! message = mailbox.Receive()
                let sender = mailbox.Sender()         
                match box message with
                | :? string as msg -> 
                    printfn "server recive a message from client：%s" msg 
                    sender <! "work!"
                | :? int as bossMsg->
                    printfn "server recive a message from server：%i" bossMsg 
                    find256 "chenhaowen;" 5 4 20 

                | _ -> ()   
                return! loop()
            }
            loop()

server <! 886
Console.ReadLine() |> ignore
//system.WhenTerminated.Wait()