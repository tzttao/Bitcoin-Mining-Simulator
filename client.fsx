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

let mutable count=1
let workers = 4
type Message =
    | WorkMsg of int


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

let configuration = 
    ConfigurationFactory.ParseString(
     @"akka {
         actor {
             provider = ""Akka.Remote.RemoteActorRefProvider, Akka.Remote""
             serializers {
                 hyperion = ""Akka.Serialization.HyperionSerializer, Akka.Serialization.Hyperion""
             }
             serialization-bindings {
                 ""System.Object"" = hyperion
             }               
         }
         remote {
             helios.tcp {
                 port = 2552
                 hostname = localhost
             }
         }
     }")
let system = ActorSystem.Create("RemoteFSharp", configuration)
let server = system.ActorSelection("akka.tcp://RemoteFSharp@10.136.32.178:9002/user/server")

let Worker (mailbox:Actor<_>) =
    let rec loop()=actor{
        let! msg = mailbox.Receive()
        let sender = mailbox.Sender()
        match msg with 
        | WorkMsg(workload) ->
                  for i in 1 .. workers do
                        printfn "Worker %i start working" i
                        let prefix="chenhaowen;"
                        let n=5
                        let k=4
                        let count= workload/workers
                        let mutable newCount= count
                        let mutable final256=""
                        //let mutable ii= 0
                        while  (not(n.Equals(0))&&newCount>0) do
                                let mutable phase=prefix
                                for i = 1 to n do
                                    phase <- phase+ char(Random().Next(32,127)).ToString()
                                let sb256=SHA_256(phase)  
                            // printfn "sssss:%i"phase
                                if check0 k sb256 then 
                                    printfn $"{phase} {phase.Length} {sb256}"
                                    final256<- phase+""+(phase.Length).ToString()+""+sb256
                                    server<! final256
                                    newCount<-newCount-1
                        printfn " Worker %i finished its job" i 
        //return! loop()
    }
    loop()

let client = 
    spawn system "client"
        <|fun mailbox ->
            let rec loop() = actor {
                let! message = mailbox.Receive()
                let sender = mailbox.Sender()
                match box message with
                | :? string as x -> 
                    printfn"client recive a message£º%s"x
                    server <! "client reply a message:ok!"
                    //print "tao;" 6 2 5
                    let worker = spawn system "Worker" Worker
                    worker <! WorkMsg(20)           
                | _ -> () 
                //return! loop()
            }
            loop()
        

client<! "start!"
Console.ReadLine()|>ignore
