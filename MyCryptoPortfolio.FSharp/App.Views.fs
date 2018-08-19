namespace MyCryptoPortfolio

open System
open Elmish.XamarinForms
open Elmish.XamarinForms.DynamicViews
open FSharp.Control
open Xamarin.Forms
open Services.SymbolService
open Newtonsoft.Json

module Views =
    
    open Consts
    open Model
    open Implementation
    open Helpers

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
                                                yield View.Picker(
                                                    title = "Currency ...",                                                                
                                                    itemsSource = availableBaseCurrencies,
                                                    selectedIndex = (availableBaseCurrencies |> List.findIndex (fun i -> i = model.SelectedBaseCurrency)),
                                                    selectedIndexChanged = (fun (i,_) -> dispatch (SetCurrentBaseCurrency (availableBaseCurrencies.[i])) ),
                                                    isEnabled = (model.SecondsWaitUntilNextCall <= 0)
                                                    )
                                                
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



