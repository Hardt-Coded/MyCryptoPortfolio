namespace MyCryptoPortfolio

open System
open Elmish.XamarinForms
open Elmish.XamarinForms.DynamicViews
open FSharp.Control
open Xamarin.Forms
open Services.SymbolService
open Newtonsoft.Json


module Implementation =

    open Services.RateService
    open Helpers
    open Services  
    open Model
    open MyCryptoPortfolio.Consts

    let storageModelId = "mycryptoportfolio"    

    let waitSendondsBetweenCalls = 10
        
    let toColor colorHex =
        Color.FromHex(colorHex)

    let fromColor (color:Color) =
        String.Format("#{0:X2}{1:X2}{2:X2}{3:X2}", color.A * 255.0 |> int, color.R * 255.0 |> int, color.G * 255.0 |> int, color.B * 255.0 |> int)

    let displayError title message =
        async {
            do! Application.Current.MainPage.DisplayAlert(
                   title ,
                   message,
                   "Ok")
               |> Async.AwaitTask    
            return ChangeBusyState(false)
        } |> Cmd.ofAsyncMsg
        

    let calculateByRate newRate entry =
        let newEntry = 
            {
                entry with  
                    // The update of the PreviousBaseCurrency is needed, for the double call in case of a currency change
                    PreviousBaseCurrencySymbol = entry.BaseCurrencySymbol
                    PreviousChangeRate = entry.ChangeRate
                    ChangeRate = newRate
                    ChangeRateDelta = newRate - entry.ChangeRate
                    AmountTarget = newRate * entry.Amount
                    AmountTargetDelta = newRate * entry.Amount - entry.Amount
            }
        newEntry
    
    let calculateByAmount newAmount entry =
        let newEntry = 
            {
                entry with                                    
                    AmountTarget = entry.ChangeRate * newAmount
                    AmountTargetDelta = entry.ChangeRate * newAmount - entry.Amount
            }
        newEntry       
    

    let getRates model =
        [
            // Returns rates or an error message
            async {
                let currentSymbols = model.Items |> List.map (fun i -> i.Symbol)
                try
                    let! rawRates = RateService.getRates [model.SelectedBaseCurrency] currentSymbols
                    return UpdateRates rawRates
                with
                | _ as e -> 
                    return DisplayErrorMessage ("Error loading Rates!",e.Message)
            
            } |> Cmd.ofAsyncMsg
            // disable loading spinner
            ChangeBusyState(false) |> Cmd.ofMsg
        ] |> Cmd.batch
        

    let printCurrency (number:decimal) symbol =
        sprintf "%s %s" (System.String.Format("{0:#,0.00}", number)) symbol

    let printRate (number:decimal) symbol =
        sprintf "%s %s" (System.String.Format("{0:#,0.00000}", number)) symbol

    let calculateTotal model =
        let total = model.Items |> List.sumBy (fun i -> i.ChangeRate * i.Amount)
        let totalDelta = total - model.TotalSum
        {model with TotalSum = total; TotalSumDelta = totalDelta}

    let showItemOptions (model:PortfolioEntry) = 
        async {
            let! option = 
                Application.Current.MainPage.DisplayActionSheet(
                       sprintf "Options for %s (%s)" model.CoinName model.Symbol,
                       "Cancel",
                       null,
                       "Edit Amount",
                       "Delete"
                       )
                    |> Async.AwaitTask
            
            match option with
            | "Cancel" -> return ChangeBusyState (false)
            | "Edit Amount" -> return OpenEditCoinPage (model)
            | "Delete" -> return RemoveItem (model.Symbol)
            | _ -> return ChangeBusyState (false)
        
        } |> Cmd.ofAsyncMsg


    let startWaitTimer dispatch =
        let mutable countDown = waitSendondsBetweenCalls
        let timer = new System.Timers.Timer 1000.
        timer.Elapsed.Subscribe (fun _ -> 
            if (countDown <= 0) then
                timer.Stop()
                dispatch (SetSecondsWaitUntilNextCall 0)
            else
                countDown <- countDown-1
                dispatch (SetSecondsWaitUntilNextCall countDown)
            ) |> ignore
        timer.Enabled <- true
        timer.Start()
        
    // Elmish - INIT
    let init () = 
        let a = 1

        let onError title message =
            async {
                do! Application.Current.MainPage.DisplayAlert(
                       title ,
                       message,
                       "Ok")
                   |> Async.AwaitTask

                let! coins = SymbolService.getAvailableCoins()
                return SetCurrentAvailableCoins (coins)            
            } |> Cmd.ofAsyncMsg
            
        let onInit () = 
            async {
                let! coins = SymbolService.getAvailableCoins()
                return SetCurrentAvailableCoins (coins)
            } |> Cmd.ofAsyncMsg

        let emptyModel =
            { Items = []
              IsLoading = true
              CurrentPage = Main
              CurrentItem = PortfolioEntry.Empty
              Coins = []
              SelectedBaseCurrency = "EUR"
              BaseCurrencies = []
              CurrentExchangeRates = []
              TotalSum = 0.0m
              TotalSumDelta = 0.0m
              MainColor = fromColor Color.DarkViolet
              SecondsWaitUntilNextCall = 0
              PreviousBaseCurrency = "EUR"
            }
        
        try 
            match Application.Current.Properties.TryGetValue storageModelId with
            | true, (:? string as json) -> 
                let loadedModel = Newtonsoft.Json.JsonConvert.DeserializeObject<Model>(json)                
                let model = 
                    {
                        emptyModel with 
                            Items = loadedModel.Items
                            SelectedBaseCurrency = loadedModel.SelectedBaseCurrency
                            PreviousBaseCurrency = loadedModel.PreviousBaseCurrency
                            BaseCurrencies = loadedModel.BaseCurrencies
                            CurrentExchangeRates = loadedModel.CurrentExchangeRates
                            TotalSum =loadedModel.TotalSum
                            TotalSumDelta = loadedModel.TotalSumDelta
                            
                    }

                // Update Color
                if (loadedModel.MainColor = null) then
                    let baseColor = fromColor Color.DarkViolet
                    let model = 
                        {
                            model with 
                                Items = model.Items 
                                |> List.map (fun item -> {item with MainColor = baseColor})
                                MainColor = baseColor
                        }
                    model, onInit()
                else
                    let model = 
                        {
                            model with 
                                Items = model.Items 
                                |> List.map (fun item -> {item with MainColor = loadedModel.MainColor})
                                MainColor = loadedModel.MainColor
                        }
                    model, onInit()    
            | _ -> emptyModel, onInit()
         with ex -> 
            emptyModel, (onError "Error restoring Data" ex.Message)

        

    let addNewItem newItem model =
        if (newItem.Amount < 0.0m) then
            model, Cmd.ofMsg (DisplayErrorMessage invalidAmount)
        elif (model.Items |> List.exists (fun i -> i.Symbol = newItem.Symbol)) then
            model, Cmd.ofMsg (DisplayErrorMessage coinAlreadyExists)
        else
            let newModel = {model with Items = newItem::model.Items}
            let newModel = calculateTotal newModel
            newModel, Cmd.ofMsg CloseItemPage    

    let updateItem item model =
        let newModel = {
            model with Items = model.Items 
                                |> List.map (fun i -> 
                                    if i.Symbol = item.Symbol then {i with Amount = item.Amount} else i
                                )}
        let newModel = calculateTotal newModel               
        newModel, Cmd.ofMsg CloseItemPage

    let removeItem symbol model =
        let newItems = model.Items |> List.filter (fun i -> i.Symbol <> symbol)
        let newModel = {model with Items = newItems}
        let newModel = calculateTotal newModel
        newModel, Cmd.none

    let setFavoriteItem symbol model =
        let newModel = {
            model with 
                Items = model.Items 
                |> List.map (fun item -> 
                    if item.Symbol = symbol then
                        {item with DisplayType = AsTab}
                    else
                        {item with DisplayType = Simple}
                    )}
        newModel, Cmd.none

    let removeFavoriteItem symbol model =
        let newModel = {
            model with 
                Items = model.Items 
                |> List.map (fun item -> 
                    if item.Symbol = symbol then
                        {item with DisplayType = Simple}
                    else
                        item
                    )}
        newModel, Cmd.none

    let displayItemOptions itemModel model =
        model, (showItemOptions itemModel)

    let changeCurrentItemSymbol (coin:Coin) model =
        let newCurrentItem = {model.CurrentItem with Symbol = coin.Symbol; CoinName = coin.Name}
        {model with CurrentItem = newCurrentItem}, Cmd.none
    
    let changeCurrentItemAmount amount model =
        let hasParsed,parsedAmount = Decimal.TryParse(amount)
        if (hasParsed && parsedAmount >= 0.0m) then
            let newCurrentItem = {model.CurrentItem with Amount = parsedAmount}
            {model with CurrentItem = newCurrentItem}, Cmd.none
        else                    
            model, Cmd.ofMsg (DisplayErrorMessage invalidAmount)

    let loadRates model =
        {model with IsLoading = true}, getRates model

    let updateRates rates model =
        match model.Items with
        | [] -> 
            model,Cmd.none
        | _ ->                 
            let getRate currency symbol = 
                rates 
                |> List.tryPick (fun i -> 
                    if i.CryptoSymbol = symbol then
                        let rate = i.Rates |> List.tryPick (fun r -> if r.CurrencySymbol = currency then Some r else None)
                        rate                           
                    else None)
            let newItems = 
                model.Items 
                |> List.map (fun item ->
                    match getRate model.SelectedBaseCurrency item.Symbol with
                    | Some rate -> 
                        let result = item |> calculateByRate rate.Rate

                        // When we change the currency, that set the rate twice, so that the delta is 0 ;)
                        if (model.SelectedBaseCurrency <> model.PreviousBaseCurrency) then
                            result |> calculateByRate rate.Rate
                        else
                            result
                    | None -> item
                    )
            
            // don't forget, that we modify the PreviousBaseCurrency, so that the next getRates doesn't zero our deltas
            let newModel = {model with Items = newItems; CurrentExchangeRates = rates; PreviousBaseCurrency = model.SelectedBaseCurrency}            
            let newModel = 
                let result = newModel  |> calculateTotal
                if (model.SelectedBaseCurrency <> model.PreviousBaseCurrency) then
                    result |> calculateTotal
                else
                    result
            let cmds = [
                Cmd.ofMsg (SetSecondsWaitUntilNextCall waitSendondsBetweenCalls)    
                Cmd.ofMsg (ChangeBusyState(false))
            ]                    

            newModel,  Cmd.batch cmds

    let setCurrentBaseCurrency currency model =
        let newModel = {
            model with
                SelectedBaseCurrency = currency
                PreviousBaseCurrency = model.SelectedBaseCurrency
                Items = model.Items |> List.map (fun i -> {i with PreviousBaseCurrencySymbol = i.BaseCurrencySymbol; BaseCurrencySymbol = currency})
        }
        newModel, Cmd.ofMsg LoadRates
    
    let openAddNewCoinPage model =
        {model with CurrentPage = AddNewCoin; CurrentItem = PortfolioEntry.Empty}, Cmd.none
    
    let openEditCoinPage item model=
        {model with CurrentPage = EditCoin; CurrentItem = item}, Cmd.none
    
    let closeItemPage model =
        {model with CurrentPage = Main; CurrentItem = PortfolioEntry.Empty}, Cmd.none

    let setCurrentAvailableCoins coins model =
        {model with Coins = coins}, Cmd.ofMsg (ChangeBusyState(false))
    
    let changeBusyState state model =
        {model with IsLoading = state}, Cmd.none
    
    let displayErrorMessage (title,message) model =
        let onError = displayError title message
        model, onError 
    
    let changeMainColor colorHex model =
        let newModel = {
            model with 
                Items = model.Items 
                |> List.map (fun item -> {item with MainColor = colorHex})
                MainColor = colorHex
        }
        newModel, Cmd.none

    let setSecondsWaitUntilNextCall seconds model =
        let newModel = {model with SecondsWaitUntilNextCall = seconds}
        // Start timer, if the wait time is set to start
        let cmd = 
            if (seconds >= waitSendondsBetweenCalls) then
                (Cmd.ofSub startWaitTimer)
            else
                Cmd.none                    
        newModel, cmd   


    // Elmish - UPDATE
    let update msg model =         
        let newModel =
            match msg with
            // Items
            | AddNewItem newItem -> model |> addNewItem newItem            
            | UpdateItem item -> model |> updateItem item
            | RemoveItem symbol -> model |> removeItem symbol                            
            | SetFavoriteItem symbol -> model |> setFavoriteItem symbol
            | RemoveFavoriteItem symbol -> model |> removeFavoriteItem symbol
            | DisplayItemOptions itemModel -> model |> displayItemOptions itemModel
            
            // Single Item
            | ChangeCurrentItemSymbol coin -> model |> changeCurrentItemSymbol coin
            | ChangeCurrentItemAmount amount -> model |> changeCurrentItemAmount amount                

            // Rates
            | LoadRates -> model |> loadRates
            | UpdateRates rates -> model |> updateRates rates
            | SetCurrentBaseCurrency currency -> model |> setCurrentBaseCurrency currency

            // Pages
            | OpenAddNewCoinPage -> model |> openAddNewCoinPage
            | OpenEditCoinPage item -> model |> openEditCoinPage item
            | CloseItemPage -> model |> closeItemPage
             
             // Combo Boxes
             | SetCurrentAvailableCoins coins -> model |> setCurrentAvailableCoins coins
            
            // Common
            | ChangeBusyState state -> model |> changeBusyState state
            | DisplayErrorMessage (title,message) ->  model |> displayErrorMessage (title,message)
            | ChangeMainColor colorHex -> model |> changeMainColor colorHex
            | SetSecondsWaitUntilNextCall seconds -> model |> setSecondsWaitUntilNextCall seconds
                 

            
        // persist current model into        
        Application.Current.Properties.[storageModelId] <- JsonConvert.SerializeObject(fst newModel)
        async {
            do! Application.Current.SavePropertiesAsync() |> Async.AwaitTask
        } |> Async.Start
        
        newModel


 

