namespace MyCryptoPortfolio

open System.Linq
open System.Numerics
open Xamarin.Forms


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
    BaseCurrencySymbol:string
    MainColor:string
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
            BaseCurrencySymbol = ""
            MainColor = ""
            }

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

    // Error Messages
    let invalidAmount = ("Error on entering the Amount","The Amount must be at least zero.")
    let coinAlreadyExists = ("Error on adding new Coin","Coin already exists in your portfolio!")


module Helpers =

    let (|LowerZero|Zero|GreaterZero|) x =
        if (x = 0.0M) then Zero
        elif (x < 0.0M) then LowerZero
        else GreaterZero