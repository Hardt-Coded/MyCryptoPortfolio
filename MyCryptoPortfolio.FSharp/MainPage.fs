namespace MyCryptoPortfolio

open Services.RateService

module MainPage =
    
    open Newtonsoft.Json
    open Xamarin.Forms
    open Helpers
    open Elmish.XamarinForms    
    open Services
    open Elmish.XamarinForms.DynamicViews

    type Model = {
        SelectedBaseCurrency:string
        PreviousBaseCurrency:string
                
        
        MainColor:string        
        IsLoading:bool        
        SecondsWaitUntilNextCall: int        
        Items: PortfolioEntry list        
        TotalSum:decimal        
        TotalSumDelta:decimal
        BaseCurrencies: string list
    }

    type Msg = 
        // Items
        | Nothing
        | AddNewItem
        | UpdateItem of PortfolioEntry
        | RemoveItem of PortfolioEntry        
        | SetFavoriteItem of PortfolioEntry
        | RemoveFavoriteItem of PortfolioEntry
        | DisplayItemOptions of PortfolioEntry

        // Rates
        | LoadRates 
        | UpdateRates of Rate list
        | SetCurrentBaseCurrency of string
        
        // Common
        | ChangeBusyState of bool
        | DisplayErrorMessage of title:string * message:string        
        | SetSecondsWaitUntilNextCall of seconds:int
        
        // Events
        | ItemRemoved of PortfolioEntry list

    type ExternalMsg =
        | Nope
        | OpenAddNewItemPage
        | OpenEditItemPage of PortfolioEntry
        






    module Helpers =       

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
            // Returns rates or an error message
            async {
                let currentSymbols = model.Items |> List.map (fun i -> i.Symbol)
                try
                    let! rawRates = RateService.getRates [model.SelectedBaseCurrency] currentSymbols
                    return [UpdateRates rawRates;ChangeBusyState false]
                with
                | _ as e -> 
                    return [DisplayErrorMessage ("Error loading Rates!",e |> extractInnerExceptionIfAvailable);ChangeBusyState false]
            
            } |> Cmd.ofAsyncBatchMsg
                
            
        

        let printCurrency (number:decimal) symbol =
            sprintf "%s %s" (System.String.Format("{0:#,0.00}", number)) symbol

        let printRate (number:decimal) symbol =
            sprintf "%s %s" (System.String.Format("{0:#,0.00000}", number)) symbol

        let calculateTotal model =
            let total = model.Items |> List.sumBy (fun i -> i.ChangeRate * i.Amount)
            let totalDelta = model.Items |> List.sumBy (fun i -> i.ChangeRateDelta * i.Amount)            
            {model with TotalSum = total; TotalSumDelta = totalDelta}
        
        let startWaitTimer dispatch =
            let mutable countDown = Consts.waitSendondsBetweenCalls
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


    module Update =
        open Helpers
        open Services
        

        let addNewItem model =
            model, Cmd.none, OpenAddNewItemPage    

        let updateItem item model =                           
            model, Cmd.none, OpenEditItemPage item 

        let removeItem item model =
            let asyncCmd =
                async {
                    do! PortfolioService.Items.deleteItemAsync item
                    let items = PortfolioService.Items.getItems()
                    return ItemRemoved items
                } |> Cmd.ofAsyncMsg
            model, asyncCmd, Nope

        let setFavoriteItem item model =
            let newItems = 
                model.Items 
                |> List.map (fun i -> 
                    if i.Symbol = item.Symbol then
                        {i with DisplayType = AsTab}
                    else
                        {i with DisplayType = Simple}
                    )            

            {model with Items = newItems}, Cmd.none, Nope  

        let removeFavoriteItem item model =
            let newItems = 
                model.Items 
                |> List.map (fun i -> 
                    if i.Symbol = item.Symbol then
                        {item with DisplayType = Simple}
                    else
                        item
                    )

            {model with Items = newItems}, Cmd.none, Nope

        let displayItemOptions item model =
            let openOptions = 
                async {
                    let! option = 
                        Application.Current.MainPage.DisplayActionSheet(
                                sprintf "Options for %s (%s)" item.CoinName item.Symbol,
                                "Cancel",
                                null,
                                "Edit Amount",
                                "Delete"
                                )
                            |> Async.AwaitTask
            
                    match option with
                    | "Cancel" -> return ChangeBusyState (false)
                    | "Edit Amount" -> return UpdateItem (item)
                    | "Delete" -> return RemoveItem (item)
                    | _ -> return ChangeBusyState (false)
                } |> Cmd.ofAsyncMsg
            
            model, openOptions, Nope

        let loadRates model =
            {model with IsLoading = true}, getRates model, Nope

        let updateRates rates model =
            match model.Items with
            | [] -> 
                model,Cmd.none, Nope
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
                let newModel = {model with Items = newItems; PreviousBaseCurrency = model.SelectedBaseCurrency}            
                let newModel = 
                    let result = newModel  |> calculateTotal
                    if (model.SelectedBaseCurrency <> model.PreviousBaseCurrency) then
                        result |> calculateTotal
                    else
                        result                

                newModel,  Cmd.ofMsg (SetSecondsWaitUntilNextCall Consts.waitSendondsBetweenCalls), Nope

        let setCurrentBaseCurrency currency model =
            let newModel = {
                model with
                    SelectedBaseCurrency = currency
                    PreviousBaseCurrency = model.SelectedBaseCurrency
                    Items = model.Items |> List.map (fun i -> {i with PreviousBaseCurrencySymbol = i.BaseCurrencySymbol; BaseCurrencySymbol = currency})
            }
            newModel, Cmd.ofMsg LoadRates, Nope

        let changeBusyState state model =
            {model with IsLoading = state}, Cmd.none, Nope
    
        let displayErrorMessage (title,message) model =
            let onError = displayError title message
            model, onError, Nope    
        

        let setSecondsWaitUntilNextCall seconds model =
            let newModel = {model with SecondsWaitUntilNextCall = seconds}
            // Start timer, if the wait time is set to start
            let cmd = 
                if (seconds >= Consts.waitSendondsBetweenCalls) then
                    (Cmd.ofSub startWaitTimer)
                else
                    Cmd.none                    
            newModel, cmd, Nope                    

    module Views =
        open Helpers

        let renderEntry mainColor (model:PortfolioEntry) dispatch =
            View.Grid(
                coldefs = [box "*";box "*"],
                rowdefs = [box "auto";box "auto";box "auto";box "auto";box "auto";box "auto"],   
                rowSpacing = 0.0,
                padding =3.0,
                children = [
                    yield View.Label(
                                text=sprintf "%s (%s)" model.CoinName model.Symbol,
                                fontAttributes = FontAttributes.Bold,
                                fontSize = 14.0,
                                horizontalOptions = LayoutOptions.Fill,
                                horizontalTextAlignment = TextAlignment.Center,
                                textColor = Color.White,
                                backgroundColor = toColor mainColor,
                                margin=0.0)
                        .GridRow(0)
                        .GridColumn(0)
                        .GridColumnSpan(2)
                    
                    yield View.Label(
                                text=(model.Amount |> sprintf "Amount: %.8M"),
                                fontAttributes = FontAttributes.Bold,
                                horizontalOptions = LayoutOptions.Fill,
                                horizontalTextAlignment = TextAlignment.Center,
                                textColor = Color.White,
                                backgroundColor = toColor mainColor,
                                margin=0.0)
                        .GridRow(1)
                        .GridColumn(0)
                        .GridColumnSpan(2)

                    yield View.StackLayout(
                        orientation=StackOrientation.Horizontal,
                        horizontalOptions = LayoutOptions.End,
                        verticalOptions = LayoutOptions.Center,
                        children = [
                            match model.DisplayType with
                            | Simple -> 
                                yield View.Label(
                                        text="⍟",
                                        textColor = Color.Gray,
                                        fontSize=38,                                    
                                        margin = Thickness (0.0, -9.0, 4.0, -5.0),
                                        gestureRecognizers = [View.TapGestureRecognizer(command = fun () -> dispatch (SetFavoriteItem model))])
                                
                            | AsTab -> 
                                yield View.Label(
                                        text="⍟",
                                        textColor = Color.Yellow,
                                        fontSize=38,                                    
                                        margin = Thickness (0.0, -9.0, 4.0, -5.0),
                                        gestureRecognizers = [View.TapGestureRecognizer(command = fun () -> dispatch (RemoveFavoriteItem model))])

                            yield View.Label(
                                    text="⚙",
                                    textColor = Color.White,
                                    fontSize=32,
                                    horizontalOptions = LayoutOptions.End,
                                    verticalOptions = LayoutOptions.Center,
                                    margin = Thickness (0.0, -5.0, 6.0, -5.0),
                                    gestureRecognizers = [View.TapGestureRecognizer(command = fun () -> dispatch (DisplayItemOptions model))])
                                
                        ]
                        ).GridRow(0).GridRowSpan(2).GridColumn(1)
                
                
                    yield View.Label(text="Rate",margin = 1.0)
                        .GridRow(2)
                        .GridColumn(0)
                    yield View.Label(text="Total",margin = 1.0)
                        .GridRow(2)
                        .GridColumn(1)                                
                
                    yield View.Label(
                            text=printRate model.ChangeRate model.BaseCurrencySymbol,
                            margin = 1.0)
                        .GridRow(3)
                        .GridColumn(0)
                    yield View.Label(
                            text=printCurrency (model.Amount * model.ChangeRate) model.BaseCurrencySymbol,
                            margin = 1.0)
                        .GridRow(3)
                        .GridColumn(1)
                                
                    yield View.Label(
                            text=printCurrency model.ChangeRateDelta model.BaseCurrencySymbol,
                            margin = 1.0,
                            textColor = 
                                match model.ChangeRateDelta with
                                | LowerZero -> Color.DarkRed
                                | Zero  -> Color.Default
                                | GreaterZero -> Color.DarkGreen)
                        .GridRow(4)
                        .GridColumn(0)
                    yield View.Label(
                            text=printCurrency (model.Amount * model.ChangeRateDelta) model.BaseCurrencySymbol,
                            margin = 1.0,
                            textColor = 
                                match model.ChangeRateDelta with
                                | LowerZero -> Color.DarkRed
                                | Zero  -> Color.Default
                                | GreaterZero -> Color.DarkGreen
                            )
                        .GridRow(4)
                        .GridColumn(1)
                
                ]
            )

        let renderMainPage model dispatch =
            View.ContentPage(
                title = "Home",
                content = View.Grid(
                            children = [
                                yield View.Grid(
                                        rowdefs = [box "auto";box "*";box "auto"],
                                        rowSpacing = 0.0,
                                        verticalOptions = LayoutOptions.Fill,
                                        children = [
                                            yield View.StackLayout(
                                                orientation = StackOrientation.Horizontal,
                                                horizontalOptions = LayoutOptions.End,
                                                children = [
                                                    yield View.Picker(
                                                        title = "Currency ...",                                                                
                                                        itemsSource = Consts.availableBaseCurrencies,
                                                        selectedIndex = (Consts.availableBaseCurrencies |> List.findIndex (fun i -> i = model.SelectedBaseCurrency)),
                                                        selectedIndexChanged = (fun (i,_) -> dispatch (SetCurrentBaseCurrency (Consts.availableBaseCurrencies.[i])) ),
                                                        isEnabled = (model.SecondsWaitUntilNextCall <= 0)
                                                        )
                                                
                                                    yield View.Label(
                                                        text="+",
                                                        fontSize=32,
                                                        fontAttributes = FontAttributes.Bold,
                                                        textColor=toColor model.MainColor,
                                                        verticalOptions = LayoutOptions.Center,
                                                        margin = 3.0,
                                                        gestureRecognizers = [View.TapGestureRecognizer(command = fun () -> dispatch(AddNewItem))]
                                                        )                                                
                                                ]
                                            ).GridRow(0)

                                            yield View.ScrollView(                        
                                                View.StackLayout(                                
                                                    horizontalOptions = LayoutOptions.Fill,
                                                    verticalOptions = LayoutOptions.Start,
                                                    children = [                                    
                                                            for entry in model.Items -> renderEntry model.MainColor entry dispatch
                                                        ]
                                                )
                                            ).GridRow(1)

                                            yield View.Grid(
                                                coldefs = [box "*";box "*"],
                                                rowdefs = [box "auto";box "auto";box "auto"],
                                                rowSpacing = 0.0,
                                                backgroundColor = toColor model.MainColor,
                                                children = [

                                                    yield View.Label(
                                                            text="Total",
                                                            margin = 1.0,
                                                            fontAttributes = FontAttributes.Bold,
                                                            textColor=Color.White)
                                                        .GridRow(0)
                                                        .GridColumn(1)

                                                    yield View.Label(
                                                            text=printCurrency (model.TotalSum) model.SelectedBaseCurrency,
                                                            margin = 1.0,
                                                            fontAttributes = FontAttributes.Bold,
                                                            textColor=Color.White)
                                                        .GridRow(1)
                                                        .GridColumn(1)
                                
                                                                
                                                    yield View.Label(
                                                            text=printCurrency (model.TotalSumDelta) model.SelectedBaseCurrency,
                                                            margin = 1.0,
                                                            fontAttributes = FontAttributes.Bold,                                                        
                                                            textColor = 
                                                                match model.TotalSumDelta with
                                                                | LowerZero -> Color.OrangeRed
                                                                | Zero  -> Color.White
                                                                | GreaterZero -> Color.GreenYellow
                                                            )
                                                        .GridRow(2)    
                                                        .GridColumn(1)


                                                    yield View.StackLayout(
                                                        orientation = StackOrientation.Horizontal,
                                                        horizontalOptions = LayoutOptions.End,
                                                        children = [
                                                        
                                                            if model.SecondsWaitUntilNextCall > 0 then    
                                                                yield View.Label(
                                                                    text=(model.SecondsWaitUntilNextCall |> sprintf "%i"),
                                                                    fontSize=24,
                                                                    fontAttributes = FontAttributes.Bold,
                                                                    verticalOptions = LayoutOptions.Center,
                                                                    textColor=Color.DarkGray,     
                                                                    margin = Thickness(2.0, 6.0, 6.0, 2.0)
                                                                    )
                                                            else
                                                                yield View.Label(
                                                                    text="⟳",
                                                                    fontSize=30,
                                                                    fontAttributes = FontAttributes.Bold,
                                                                    verticalOptions = LayoutOptions.Center,
                                                                    textColor=Color.White,     
                                                                    margin = 3.0,
                                                                    gestureRecognizers = [ View.TapGestureRecognizer(command = fun () -> dispatch(LoadRates)) ]
                                                                    )
                                                        ]
                                                        )
                                                        .GridRow(0)    
                                                        .GridColumn(1)
                                                        .GridRowSpan(3)
                                                ]
                                            ).GridRow(2)

                                        ]
                                )
                            

                                if model.IsLoading then 
                                    yield CommonViews.createBusyLayer()
                            ]
                        
                        )
                )


    open Update
    open Views   
    
    let init mainColor = 
        
        let loadedMainViewModel = PortfolioService.MainViewModel.getMainViewModel ()
        let items = PortfolioService.Items.getItems ()

        let emptyModel =
            {   Items = items
                IsLoading = false
                BaseCurrencies = []                
                TotalSum = 0.0m
                TotalSumDelta = 0.0m
                MainColor = mainColor
                
                SelectedBaseCurrency = loadedMainViewModel.SelectedBaseCurrency
                PreviousBaseCurrency = loadedMainViewModel.PreviousBaseCurrency
                SecondsWaitUntilNextCall = 0
            }
        
        Helpers.calculateTotal emptyModel, Cmd.none

    let update msg model =         
        let newModel =
            match msg with
            // Items
            | AddNewItem -> model |> addNewItem            
            | UpdateItem item -> model |> updateItem item
            | RemoveItem symbol -> model |> removeItem symbol                            
            | SetFavoriteItem item -> model |> setFavoriteItem item
            | RemoveFavoriteItem item -> model |> removeFavoriteItem item
            | DisplayItemOptions item -> model |> displayItemOptions item            
            
            // Rates
            | LoadRates -> model |> loadRates
            | UpdateRates rates -> model |> updateRates rates
            | SetCurrentBaseCurrency currency -> model |> setCurrentBaseCurrency currency                        
             
            // Common
            | ChangeBusyState state -> model |> changeBusyState state
            | DisplayErrorMessage (title,message) ->  model |> displayErrorMessage (title,message)            
            | SetSecondsWaitUntilNextCall seconds -> model |> setSecondsWaitUntilNextCall seconds
            | Nothing -> model, Cmd.none, Nope
            // Events
            | ItemRemoved items -> {model with Items = items}, Cmd.none, Nope

        // Store data            
        async {
            do! PortfolioService.Items.storeItemsAsync model.Items
            do! PortfolioService.MainViewModel.storeMainViewModelAsync {SelectedBaseCurrency = model.SelectedBaseCurrency;PreviousBaseCurrency = model.PreviousBaseCurrency}
        } |> Async.StartImmediate

        newModel

    let view  = renderMainPage
        
            