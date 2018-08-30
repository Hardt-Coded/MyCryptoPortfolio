namespace MyCryptoPortfolio

open System.Linq
open System.Numerics
open Xamarin.Forms

exception Ex of string    

/// Represents items in the model
type Coin = {Symbol:string;Name:string}

type DisplayType =
    | Simple
    | AsTab
    


type PortfolioEntry = {
    Symbol:string
    CoinName:string
    Amount:decimal
    
    PreviousChangeRate:decimal
    ChangeRate:decimal
    ChangeRateDelta:decimal
    AmountTarget:decimal
    AmountTargetDelta:decimal
    DisplayType:DisplayType
    PreviousBaseCurrencySymbol:string
    BaseCurrencySymbol:string    
    }
    with
        static member Empty = {
            Symbol = ""
            CoinName = ""
            Amount = 0.0m
            PreviousChangeRate = 0.0m
            ChangeRate = 1.0m
            ChangeRateDelta = 0.0m
            AmountTarget = 0.0m
            AmountTargetDelta = 0.0m
            DisplayType = Simple
            PreviousBaseCurrencySymbol = ""
            BaseCurrencySymbol = ""            
            }

[<RequireQualifiedAccess>]
module Consts =

    let availableMainColors = [
        Color.DarkViolet
        Color.DarkBlue
        Color.DarkCyan
        Color.DarkGoldenrod
        Color.DarkGray
        Color.DarkGreen
        Color.DarkKhaki
        Color.DarkMagenta
        Color.DarkOliveGreen
        Color.DarkOrange
        Color.DarkOrchid
        Color.DarkRed
        Color.DarkSalmon
        Color.DarkSeaGreen
        Color.DarkSlateBlue
        Color.DarkSlateGray
        Color.DarkTurquoise
        Color.Blue
        Color.BlueViolet
        Color.Black
    ]

    let availableBaseCurrencies = ["EUR"; "USD"; "JPY"; "GBP";"AUD";]

    // Error Messages
    let invalidAmount = ("Error on entering the Amount","The Amount must be at least zero.")
    let coinAlreadyExists = ("Error on adding new Coin","Coin already exists in your portfolio!")

    let storageModelId = "mycryptoportfolio"
    let mainviewStorageModelId = storageModelId + "mainview"
    let entryStorageModelId = storageModelId + "entries"
    let mainColorStorageModelId = storageModelId + "maincolor"
    

    let waitSendondsBetweenCalls = 10


module Helpers =
    open System

    let (|LowerZero|Zero|GreaterZero|) x =
        if (x = 0.0M) then Zero
        elif (x < 0.0M) then LowerZero
        else GreaterZero

    let toColor colorHex =
        Color.FromHex(colorHex)

    let fromColor (color:Color) =
        String.Format("#{0:X2}{1:X2}{2:X2}{3:X2}", color.A * 255.0 |> int, color.R * 255.0 |> int, color.G * 255.0 |> int, color.B * 255.0 |> int)

    
    let extractInnerExceptionIfAvailable (ex:exn) =
        match ex with
        | Ex msg -> msg
        | _ -> if (ex.InnerException = null) then ex.Message else ex.InnerException.Message

module CommonViews =
    open Elmish.XamarinForms.DynamicViews

    let createBusyLayer () =
        View.Grid(
            backgroundColor = Color.FromHex "#A0000000",
            children = [
                View.ActivityIndicator(
                    isRunning = true,
                    color = Color.White,
                    scale = 0.1
                )
            ]
        )