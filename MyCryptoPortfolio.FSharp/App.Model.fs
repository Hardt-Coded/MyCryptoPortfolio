namespace MyCryptoPortfolio

open System
open Elmish.XamarinForms
open Elmish.XamarinForms.DynamicViews
open FSharp.Control
open Xamarin.Forms
open Services.SymbolService
open Newtonsoft.Json

module Model =
    open Services.RateService



    type CurrentPage = Main | AddNewCoin | EditCoin

    type ItemPageType = AddNew | Edit

    type Model = {
            Items: PortfolioEntry list
            SelectedBaseCurrency:string
            BaseCurrencies: string list
            CurrentExchangeRates: Rate list
            TotalSum:decimal
            TotalSumDelta:decimal
            MainColor:string

            [<JsonIgnore>]
            IsLoading:bool

            [<JsonIgnore>]
            CurrentPage:CurrentPage

            [<JsonIgnore>]
            CurrentItem:PortfolioEntry

            [<JsonIgnore>]
            Coins: Coin list

            [<JsonIgnore>]
            SecondsWaitUntilNextCall: int
        }

    type Msg = 
        // Items
        | AddNewItem of PortfolioEntry
        | UpdateItem of PortfolioEntry
        | RemoveItem of symbol:string
        | SetFavoriteItem of symbol:string
        | RemoveFavoriteItem of symbol:string
        | DisplayItemOptions of model:PortfolioEntry

        // Single Item
        | ChangeCurrentItemSymbol of Coin
        | ChangeCurrentItemAmount of string        

        // Rates
        | LoadRates 
        | UpdateRates of Rate list
        
        // Pages
        | OpenAddNewCoinPage
        | OpenEditCoinPage of PortfolioEntry        
        | CloseItemPage
        
        // Combo Boxes
        | SetCurrentAvailableCoins of Coin list

        // Common
        | ChangeBusyState of bool
        | DisplayErrorMessage of title:string * message:string
        | ChangeMainColor of colorHex:string
        | SetSecondsWaitUntilNextCall of seconds:int
    



