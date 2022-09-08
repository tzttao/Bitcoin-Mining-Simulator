// Learn more about F# at http://fsharp.org
#time "on"
#r "nuget: Akka.FSharp" 
#r "nuget: Akka.TestKit"
#r "nuget: Akka.Remote"
#r "nuget: Akka.Serialization.Hyperion"

open System
open Akka.Actor
open Akka.FSharp
open System.Security.Cryptography; 
let system = System.create "system" (Configuration.load())
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
      while  (not(n.Equals(0))&&newCount>0) do
                let mutable phase=prefix
                for i = 1 to n do
                    phase <- phase+ char(Random().Next(32,127)).ToString()
                let sb256=SHA_256(phase)  
           // printfn "sssss:%i"phase
                if check0 k sb256 then 
                    printfn $"{phase} {phase.Length} {sb256}"
                    newCount<-newCount-1

let workers=2
let mutable count=0

type Message =
    | WorkerMsg of string*int*int*int
    | StartMsg of string
    | EndMsg of string

let FindSha256 (mailbox:Actor<_>)=
    let rec loop()=actor{
        let! msg = mailbox.Receive()
        match msg with
        | WorkerMsg(prefix,n,k,count) -> 
                    find256 prefix n k count//"chenhaowen;", 5, 4, 10
                    mailbox.Sender()<! EndMsg("done!")
        | _ -> printfn "Worker Received Wrong message"
 
    }
    loop()


let Worker (mailbox:Actor<_>) =
    let rec loop()=actor{
        let! msg = mailbox.Receive()
        match msg with 
        | StartMsg(start) ->    
                let workersList=[for a in 1 .. workers do yield(spawn system (string a) FindSha256)]                             
                for i in 0 .. (workers-1) do
                     workersList.Item(i|>int) <! WorkerMsg("chenhaowen;", 5, 4, 10)                                       
        | EndMsg (msg) ->            
                count <- count+1
                if count = workers then
                   mailbox.Context.System.Terminate() |> ignore
        | _ -> printfn "wrong message"
        return! loop()
    }
    loop()

let WorkerRef = spawn system "Worker" Worker
WorkerRef <! StartMsg("start")
system.WhenTerminated.Wait()



