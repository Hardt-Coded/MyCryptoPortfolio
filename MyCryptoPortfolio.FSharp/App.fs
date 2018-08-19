namespace MyCryptoPortfolio

open System
open Elmish.XamarinForms
open Elmish.XamarinForms.DynamicViews
open FSharp.Control
open Xamarin.Forms
open Services.SymbolService
open Newtonsoft.Json



module App =
    open Model
    open Implementation

    
    
    let program = Program.mkProgram init update view

type App () as app = 
    inherit Application ()

    let runner = 
        App.program
#if DEBUG
        |> Program.withConsoleTrace
#endif
        |> Program.runWithDynamicView app

#if DEBUG    
    do runner.EnableLiveUpdate()
#endif    

   




