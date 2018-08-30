namespace MyCryptoPortfolio

[<AutoOpen>]
module Cmd = 
    open Elmish.XamarinForms
    
    // 
    let ofAsyncBatchMsg (p: Async<#seq<'msg>>) : Cmd<'msg> =
        [ fun dispatch -> async { let! msgs = p in msgs |> Seq.iter dispatch } |> Async.StartImmediate ]

