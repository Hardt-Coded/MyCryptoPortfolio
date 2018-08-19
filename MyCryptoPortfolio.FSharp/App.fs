namespace MyCryptoPortfolio.FSharp

open System
open Elmish.XamarinForms
open Elmish.XamarinForms.DynamicViews
open FSharp.Control
open Xamarin.Forms
open Services.SymbolService
open Newtonsoft.Json



module App =
    open Services.RateService
    open Helpers
    open Services
    open Xamarin.Forms
    open Xamarin.Forms

    let storageModelId = "mycryptoportfolio"

    // Error Messages
    let invalidAmount = ("Error on entering the Amount","The Amount must be at least zero.")
    let coinAlreadyExists = ("Error on adding new Coin","Coin already exists in your portfolio!")

    type CurrentPage = Main | AddNewCoin | EditCoin

    type ItemPageType = AddNew | Edit

    type Model = {
            Items: PortfolioEntry list
            SelectedBaseCurrency:string
            BaseCurrencies: string list
            CurrentExchangeRates: Rate list
            TotalSum:decimal
            TotalSumDelta:decimal

            [<JsonIgnore>]
            IsLoading:bool

            [<JsonIgnore>]
            CurrentPage:CurrentPage

            [<JsonIgnore>]
            CurrentItem:PortfolioEntry

            [<JsonIgnore>]
            Coins: Coin list
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
        async {
            let currentSymbols = model.Items |> List.map (fun i -> i.Symbol)
            try
                let! rawRates = RateService.getRates ["EUR";"USD"] currentSymbols
                return UpdateRates rawRates
            with
            | _ as e -> 
                return DisplayErrorMessage ("Error loading Rates!",e.Message)
            
        } |> Cmd.ofAsyncMsg
        

    let printCurrency (number:decimal) symbol =
        sprintf "%s %s" (System.String.Format("{0:#,0.00}", number)) symbol

    let printRate (number:decimal) symbol =
        sprintf "%s %s" (System.String.Format("{0:#,0.00000}", number)) symbol

    let calculateTotal model =
        let total = model.Items |> List.sumBy (fun i -> i.ChangeRate * i.Amount)
        let totalDelta = total - model.TotalSum
        {model with TotalSum = total; TotalSumDelta = totalDelta}

    let displayItemOptions (model:PortfolioEntry) = 
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

    // Combine the init logic 
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
                            BaseCurrencies = loadedModel.BaseCurrencies
                            CurrentExchangeRates = loadedModel.CurrentExchangeRates
                            TotalSum =loadedModel.TotalSum
                            TotalSumDelta = loadedModel.TotalSumDelta
                    }
                model, onInit()
            | _ -> emptyModel, onInit()
         with ex -> 
            emptyModel, (onError "Error restoring Data" ex.Message)

        

    // Combine the update logic and reconcile the external messages
    let update msg model =         
        let newModel =
            match msg with
            | AddNewItem newItem -> 
                if (newItem.Amount < 0.0m) then
                    model, Cmd.ofMsg (DisplayErrorMessage invalidAmount)
                elif (model.Items |> List.exists (fun i -> i.Symbol = newItem.Symbol)) then
                    model,Cmd.ofMsg (DisplayErrorMessage coinAlreadyExists)
                else
                    let newModel = {model with Items = newItem::model.Items}
                    let newModel = calculateTotal newModel
                    newModel, Cmd.ofMsg CloseItemPage
            
            | UpdateItem item -> 
                                
                let newModel = {
                    model with Items = model.Items 
                                       |> List.map (fun i -> 
                                            if i.Symbol = item.Symbol then {i with Amount = item.Amount} else i
                                       )}
                let newModel = calculateTotal newModel               
                newModel, Cmd.ofMsg CloseItemPage

            | RemoveItem symbol ->
                let newItems = model.Items |> List.filter (fun i -> i.Symbol <> symbol)
                let newModel = {model with Items = newItems}
                let newModel = calculateTotal newModel
                newModel, Cmd.none

            | DisplayItemOptions itemModel ->
                model, (displayItemOptions itemModel)

            | LoadRates ->                 
                {model with IsLoading = true}, getRates model

            | UpdateRates rates -> 
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
                            | Some rate -> item |> calculateByRate rate.Rate
                            | None -> item
                            )
                    let newModel = {model with Items = newItems; CurrentExchangeRates = rates}
                    let newModel = calculateTotal newModel
                    newModel, Cmd.ofMsg (ChangeBusyState(false))

            | ChangeBusyState b -> 
                {model with IsLoading = b}, Cmd.none

            | OpenAddNewCoinPage -> 
                {model with CurrentPage = AddNewCoin; CurrentItem = PortfolioEntry.Empty}, Cmd.none

            | CloseItemPage -> 
                {model with CurrentPage = Main; CurrentItem = PortfolioEntry.Empty}, Cmd.none

            | OpenEditCoinPage item -> 
                {model with CurrentPage = EditCoin; CurrentItem = item}, Cmd.none            

            | ChangeCurrentItemSymbol coin ->                 
                let newCurrentItem = {model.CurrentItem with Symbol = coin.Symbol; CoinName = coin.Name}
                {model with CurrentItem = newCurrentItem}, Cmd.none
               
            | ChangeCurrentItemAmount amount ->
                let hasParsed,parsedAmount = Decimal.TryParse(amount)
                if (hasParsed && parsedAmount >= 0.0m) then
                    let newCurrentItem = {model.CurrentItem with Amount = parsedAmount}
                    {model with CurrentItem = newCurrentItem}, Cmd.none
                else                    
                    model, Cmd.ofMsg (DisplayErrorMessage invalidAmount)

            | SetCurrentAvailableCoins coins ->
                {model with Coins = coins}, Cmd.ofMsg (ChangeBusyState(false))

            | DisplayErrorMessage (title,message) ->  
                let onError = displayError title message
                model, onError
            
            | SetFavoriteItem symbol ->
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
            
            | RemoveFavoriteItem symbol ->
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

            
                
        Application.Current.Properties.[storageModelId] <- JsonConvert.SerializeObject(fst newModel)
        async {
            do! Application.Current.SavePropertiesAsync() |> Async.AwaitTask
        } |> Async.Start
        //let serializedModel = JsonConvert.SerializeObject(newModel)
        //Application.Current.Properties.["MyCurrentPortfolio"] <- serializedModel
        //async {
        //    do! Application.Current.SavePropertiesAsync() |> Async.AwaitTask
        //} |> Async.Start
        newModel


    let createBusyLayer () =
        View.Grid(
            backgroundColor = Color.FromHex "#A0000000",
            children = [
                View.ActivityIndicator(
                    isRunning = true,
                    color = Color.White,
                    scale = 0.3
                )
            ]
        )

    let renderEntry (model:PortfolioEntry) dispatch =
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
                         backgroundColor = Color.DarkViolet,
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
                         backgroundColor = Color.DarkViolet,
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
                                    gestureRecognizers = [View.TapGestureRecognizer(command = fun () -> dispatch (SetFavoriteItem model.Symbol))])
                                
                        | AsTab -> 
                            yield View.Label(
                                    text="⍟",
                                    textColor = Color.Yellow,
                                    fontSize=38,                                    
                                    margin = Thickness (0.0, -9.0, 4.0, -5.0),
                                    gestureRecognizers = [View.TapGestureRecognizer(command = fun () -> dispatch (RemoveFavoriteItem model.Symbol))])

                        yield View.Label(
                                text="⚙",
                                textColor = Color.White,
                                fontSize=32,
                                horizontalOptions = LayoutOptions.End,
                                verticalOptions = LayoutOptions.Center,
                                margin = Thickness (0.0, -4.0, 6.0, -5.0),
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
              

                yield View.StackLayout(
                    orientation=StackOrientation.Horizontal,
                    horizontalOptions = LayoutOptions.End,
                    children = [
                        yield View.Label(
                            text="✘",
                            textColor = Color.Red,
                            fontSize=32,
                            margin=3.0,
                            gestureRecognizers = [View.TapGestureRecognizer(command=fun () -> dispatch (RemoveItem model.Symbol))]
                        )

                        yield View.Label(
                            text="✎",
                            textColor = Color.DarkViolet,
                            fontAttributes = FontAttributes.Bold,
                            fontSize=38,
                            margin= Thickness (10.0, 0.0, 0.0, 0.0),
                            gestureRecognizers = [View.TapGestureRecognizer(command=fun () -> dispatch (OpenEditCoinPage model))]
                        )
                    ]
                    ).GridRow(5).GridColumn(1)                
                
            ]
        )
    
    let renderItemPage pageType title okayCommand model dispatch =
        View.ContentPage(
            title = title,
            content = View.StackLayout(
                    padding=20.0,
                    horizontalOptions = LayoutOptions.Fill,
                    verticalOptions = LayoutOptions.CenterAndExpand,
                    children = [
                            yield View.Label(text = title)
                            match pageType with
                            | AddNew ->
                                yield View.Picker(
                                        title = "Choose Coin ...",                                                                
                                        itemsSource = (model.Coins |> List.map (fun i -> sprintf "%s (%s)" i.Symbol i.Name)),
                                        selectedIndexChanged = (fun (i,_) -> dispatch (ChangeCurrentItemSymbol (model.Coins.[i])) )
                                        )
                            | Edit ->
                                yield View.Entry(
                                    text =  (sprintf "%s (%s)" model.CurrentItem.CoinName model.CurrentItem.Symbol),
                                    horizontalOptions=LayoutOptions.Fill,
                                    isEnabled = false
                                )                                
                                

                            
                            yield View.Entry(
                                text =  sprintf "%.8M" model.CurrentItem.Amount,
                                horizontalOptions=LayoutOptions.Fill,
                                completed = (fun amount -> dispatch (ChangeCurrentItemAmount amount))
                                )
                            yield View.Button(
                                text="Okay",
                                textColor=Color.White,
                                backgroundColor = Color.DarkViolet,
                                command = okayCommand
                                )
                            yield View.Button(
                                text="Cancel",
                                textColor=Color.White,
                                backgroundColor = Color.DarkViolet,
                                command = (fun () -> dispatch (CloseItemPage) )
                                )
                        ]
                )
        )


    let renderAddNewItemPage model dispatch = 
        let okayCommand = (fun () -> dispatch (AddNewItem model.CurrentItem) )        
        renderItemPage AddNew "Add a new Coin ..."  okayCommand model dispatch

    let renderEditItemPage model dispatch = 
        let okayCommand = (fun () -> dispatch (UpdateItem model.CurrentItem) )        
        renderItemPage Edit "Edit a Coin ..." okayCommand model dispatch
        

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
                                                View.Label(
                                                    text="+",
                                                    fontSize=32,
                                                    fontAttributes = FontAttributes.Bold,
                                                    textColor=Color.DarkViolet,                                                    
                                                    margin = 3.0,
                                                    gestureRecognizers = [View.TapGestureRecognizer(command = fun () -> dispatch(OpenAddNewCoinPage))]                                                    
                                                    )
                                                    
                                                View.Label(
                                                    text="⟳",
                                                    fontSize=30,
                                                    fontAttributes = FontAttributes.Bold,
                                                    textColor=Color.DarkViolet,                                                    
                                                    margin = 3.0,
                                                    gestureRecognizers = [View.TapGestureRecognizer(command = fun () -> dispatch(LoadRates))]
                                                    )
                                            ]
                                        ).GridRow(0)

                                        yield View.ScrollView(                        
                                            View.StackLayout(                                
                                                horizontalOptions = LayoutOptions.Fill,
                                                verticalOptions = LayoutOptions.Start,
                                                children = [                                    
                                                        for entry in model.Items -> renderEntry entry dispatch
                                                    ]
                                            )
                                        ).GridRow(1)

                                        yield View.Grid(
                                            coldefs = [box "*";box "*"],
                                            rowdefs = [box "auto";box "auto";box "auto"],
                                            rowSpacing = 0.0,
                                            backgroundColor = Color.DarkViolet,
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
                                            ]
                                        ).GridRow(2)

                                    ]
                            )
                            

                            if model.IsLoading then 
                                yield createBusyLayer()
                        ]
                        
                   )
            )

    let renderAboutPage model dispatch =
        View.ContentPage(
            title = "About",
            content = View.StackLayout(
                    padding=20.0,
                    horizontalOptions = LayoutOptions.Center,
                    verticalOptions = LayoutOptions.CenterAndExpand,
                    children = [
                            yield View.Label(text = "Aboooout!")
                            if model.IsLoading then 
                                yield createBusyLayer()
                        ]
                )
            )

    
    let renderFavorits model dispatch =
        model.Items 
        |> List.filter (fun x -> x.DisplayType = AsTab)        

    let view model dispatch = 
        let currentMainPage = 
            match model.CurrentPage with
            | Main -> renderMainPage model dispatch
            | AddNewCoin -> renderAddNewItemPage model dispatch
            | EditCoin -> renderEditItemPage model dispatch

        View.TabbedPage(title = "MyCryptoPortfolio",            
            barBackgroundColor = Color.DarkViolet,            
            children = 
                [
                    yield currentMainPage           
                    for entry in renderFavorits model dispatch ->
                        View.ContentPage(
                            title= sprintf "%s" entry.Symbol,                            
                            content = View.Grid(
                                    children = [
                                        yield renderEntry entry dispatch                                                    

                                        if model.IsLoading then 
                                            yield createBusyLayer()
                                    ]
                        
                                )
                            )
                    yield renderAboutPage model dispatch
                ])
    
    
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

    
    //override __.OnSleep() = 

    //    let json = Newtonsoft.Json.JsonConvert.SerializeObject(runner.CurrentModel)
    //    Console.WriteLine("OnSleep: saving model into app.Properties, json = {0}", json)

    //    app.Properties.[App.storageModelId] <- json

    //override __.OnResume() = 
    //    Console.WriteLine "OnResume: checking for model in app.Properties"
    //    try 
    //        match app.Properties.TryGetValue App.storageModelId with
    //        | true, (:? string as json) -> 

    //            Console.WriteLine("OnResume: restoring model from app.Properties, json = {0}", json)
    //            let model = Newtonsoft.Json.JsonConvert.DeserializeObject<App.Model>(json)

    //            Console.WriteLine("OnResume: restoring model from app.Properties, model = {0}", (sprintf "%0A" model))
    //            runner.SetCurrentModel (model, Cmd.none)

    //        | _ -> ()
    //    with ex -> 
    //        App.program.onError("Error while restoring model found in app.Properties", ex)

    //override this.OnStart() = 
    //    Console.WriteLine "OnStart: using same logic as OnResume()"
    //    this.OnResume()



