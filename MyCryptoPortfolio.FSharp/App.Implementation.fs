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
                    let! rawRates = RateService.getRates ["EUR";"USD"] currentSymbols
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

        

    // Elmish - UPDATE
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
                    

                    let cmds = [
                        Cmd.ofMsg (SetSecondsWaitUntilNextCall waitSendondsBetweenCalls)    
                        Cmd.ofMsg (ChangeBusyState(false))
                    ]                    

                    newModel,  Cmd.batch cmds

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

            | ChangeMainColor colorHex ->                
                let newModel = {
                    model with 
                        Items = model.Items 
                        |> List.map (fun item -> {item with MainColor = colorHex})
                        MainColor = colorHex
                }
                newModel, Cmd.none

            | SetSecondsWaitUntilNextCall seconds ->
                let newModel = {model with SecondsWaitUntilNextCall = seconds}

                // Start timer, if the wait time is set to start
                let cmd = 
                    if (seconds >= waitSendondsBetweenCalls) then
                        (Cmd.ofSub startWaitTimer)
                    else
                        Cmd.none
                    
                newModel, cmd    

            
        // persist current model into        
        Application.Current.Properties.[storageModelId] <- JsonConvert.SerializeObject(fst newModel)
        async {
            do! Application.Current.SavePropertiesAsync() |> Async.AwaitTask
        } |> Async.Start
        
        newModel


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
                         backgroundColor = toColor model.MainColor,
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
                         backgroundColor = toColor model.MainColor,
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
    
    let renderMainColorPicker model dispatch = 
        let rowCount = availableMainColors.Length / 5 |> int
        View.Grid(
            coldefs = [box "*";box "*";box "*";box "*";box "*"],
            rowdefs = [for _ in [0..rowCount - 1] do yield box "auto"],
            children = [
                for x in [0..4] do
                    for y in [0..rowCount - 1] do
                        let currentIndex = 5 * y + x
                        if (currentIndex < availableMainColors.Length) then
                            yield View.Button(
                                    text=sprintf "%i" currentIndex,
                                    textColor = Color.White,
                                    backgroundColor = availableMainColors.[currentIndex],
                                    command = fun () -> dispatch (ChangeMainColor (fromColor availableMainColors.[currentIndex]))
                                )
                                .GridColumn(x)
                                .GridRow(y)            
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
                                backgroundColor = toColor model.MainColor,
                                command = okayCommand
                                )
                            yield View.Button(
                                text="Cancel",
                                textColor=Color.White,
                                backgroundColor = toColor model.MainColor,
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
                                                yield View.Label(
                                                    text="+",
                                                    fontSize=32,
                                                    fontAttributes = FontAttributes.Bold,
                                                    textColor=toColor model.MainColor,
                                                    verticalOptions = LayoutOptions.Center,
                                                    margin = 3.0,
                                                    gestureRecognizers = [View.TapGestureRecognizer(command = fun () -> dispatch(OpenAddNewCoinPage))]
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
                                yield createBusyLayer()
                        ]
                        
                   )
            )

    let renderSettingPage model dispatch =
        View.ContentPage(
            title = "Settings",
            content = View.StackLayout(
                    padding=20.0,
                    horizontalOptions = LayoutOptions.Center,
                    verticalOptions = LayoutOptions.CenterAndExpand,
                    children = [
                            yield View.StackLayout(
                                orientation = StackOrientation.Vertical,
                                horizontalOptions = LayoutOptions.Fill,
                                verticalOptions = LayoutOptions.CenterAndExpand,
                                children = [
                                    yield View.Label(text = "Choose Main Color:")    
                                    yield renderMainColorPicker model dispatch
                                ]
                            )
                            

                            if model.IsLoading then 
                                yield createBusyLayer()
                        ]
                )
            )

    
    let renderFavorits model dispatch =
        model.Items 
        |> List.filter (fun x -> x.DisplayType = AsTab)        

    
    // ELMISH - VIEW
    let view model dispatch = 
        let currentMainPage = 
            match model.CurrentPage with
            | Main -> renderMainPage model dispatch
            | AddNewCoin -> renderAddNewItemPage model dispatch
            | EditCoin -> renderEditItemPage model dispatch

        View.TabbedPage(title = "MyCryptoPortfolio",            
            barBackgroundColor = toColor model.MainColor,            
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
                    yield renderSettingPage model dispatch
                ])

