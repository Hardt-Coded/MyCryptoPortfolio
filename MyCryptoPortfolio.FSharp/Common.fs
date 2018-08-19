namespace MyCryptoPortfolio.FSharp

open System.Linq
open System.Numerics


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
            }

module Helpers =

    let (|LowerZero|Zero|GreaterZero|) x =
        if (x = 0.0M) then Zero
        elif (x < 0.0M) then LowerZero
        else GreaterZero