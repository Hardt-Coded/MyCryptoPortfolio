namespace MyCryptoPortfolio

module ItemPage =

    
    open Xamarin.Forms
    open Elmish.XamarinForms    
    open Services
    open Elmish.XamarinForms.DynamicViews
        
    type ItemPageType = AddNew | Edit

    type Model = {
            Coins: Coin list
            IsLoading:bool            
            Item:PortfolioEntry
            PageType:ItemPageType
            MainColor:string
        }

    type Msg = 
        // Single Item
        | ChangeCurrentItemSymbol of Coin
        | ChangeCurrentItemAmount of string
        // Common
        | ChangeBusyState of bool
        | DisplayErrorMessage of title:string * message:string       
        // Combo Boxes
        | SetCurrentAvailableCoins of Coin list
        // Main Commands
        | AddNewItem
        | UpdateItem
        | Cancel
        // Events
        | ItemAdded
        | ItemEdited
        

    type ExternalMsg = 
        | Nope
        | CloseItemPage


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


    module Update =

        open System
        open Helpers         

        let changeCurrentItemSymbol (coin:Coin) model =
            let newCurrentItem = {model.Item with Symbol = coin.Symbol; CoinName = coin.Name}
            {model with Item = newCurrentItem}, Cmd.none, Nope
    
        let changeCurrentItemAmount amount model =
            let hasParsed,parsedAmount = Decimal.TryParse(amount)
            if (hasParsed && parsedAmount >= 0.0m) then
                let newCurrentItem = {model.Item with Amount = parsedAmount}
                {model with Item = newCurrentItem}, Cmd.none, Nope
            else                    
                model, Cmd.ofMsg (DisplayErrorMessage Consts.invalidAmount), Nope



        let changeBusyState state model =
            {model with IsLoading = state}, Cmd.none, Nope
    
        let displayErrorMessage (title,message) model =
            let onError = displayError title message
            model, onError, Nope


        let setCurrentAvailableCoins coins model =
            {model with Coins = coins}, Cmd.ofMsg (ChangeBusyState(false)), Nope

        let addItem model =
            let onAdd = 
                async {
                    do! PortfolioService.Items.addItemsAsync model.Item
                    return ItemAdded
                } |> Cmd.ofAsyncMsg
            model, onAdd, Nope

        let editItem model =
            let onAdd = 
                async {
                    do! PortfolioService.Items.updateItemAsync model.Item
                    return ItemAdded
                } |> Cmd.ofAsyncMsg
            model, onAdd, Nope

    module Views =

        let private renderItemPage pageType title okayCommand model dispatch =
            View.ContentPage(
                title = title,
                content = View.Grid(
                    children = [
                        yield View.StackLayout(
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
                                            text =  (sprintf "%s (%s)" model.Item.CoinName model.Item.Symbol),
                                            horizontalOptions=LayoutOptions.Fill,
                                            isEnabled = false
                                        )                                
                                

                            
                                    yield View.Entry(
                                        text =  sprintf "%.8M" model.Item.Amount,
                                        horizontalOptions=LayoutOptions.Fill,
                                        completed = (fun amount -> dispatch (ChangeCurrentItemAmount amount))
                                        )
                                    yield View.Button(
                                        text="Okay",
                                        textColor=Color.White,
                                        backgroundColor = Helpers.toColor model.MainColor,
                                        command = okayCommand
                                        )
                                    yield View.Button(
                                        text="Cancel",
                                        textColor=Color.White,
                                        backgroundColor = Helpers.toColor model.MainColor,
                                        command = (fun () -> dispatch (Cancel) )
                                        )
                                ]
                        )

                        if model.IsLoading then 
                            yield CommonViews.createBusyLayer()
                    ]
                )
                
                
                
                
            )

        let renderAddNewItemPage model dispatch = 
            let okayCommand = (fun () -> dispatch (AddNewItem) )        
            renderItemPage AddNew "Add a new Coin ..."  okayCommand model dispatch

        let renderEditItemPage model dispatch = 
            let okayCommand = (fun () -> dispatch (UpdateItem) )        
            renderItemPage Edit "Edit a Coin ..." okayCommand model dispatch        



    open Update
    open Views

    let init pagetype mainColor = 
            
        let onInit () = 
            async {
                let! coins = SymbolService.getAvailableCoins()
                return SetCurrentAvailableCoins (coins)
            } |> Cmd.ofAsyncMsg

        let emptyModel =
            { 
              IsLoading = true              
              Item = PortfolioEntry.Empty
              PageType = pagetype
              Coins = []                            
              MainColor = mainColor              
            }
        
        let busiStateCmd = ChangeBusyState(true) |> Cmd.ofMsg

        emptyModel, Cmd.batch [busiStateCmd; onInit()]

    let update msg model =         
        let newModel =
            match msg with                        
            | ChangeCurrentItemSymbol coin -> model |> changeCurrentItemSymbol coin
            | ChangeCurrentItemAmount amount -> model |> changeCurrentItemAmount amount                            
            // Common
            | ChangeBusyState state -> model |> changeBusyState state
            | DisplayErrorMessage (title,message) ->  model |> displayErrorMessage (title,message)

            | SetCurrentAvailableCoins coins -> model |> setCurrentAvailableCoins coins

            | AddNewItem -> model |> addItem
            | UpdateItem -> model |> editItem
            | Cancel -> model, Cmd.none, CloseItemPage

            | ItemAdded -> model, Cmd.none, CloseItemPage
            | ItemEdited -> model, Cmd.none, CloseItemPage
        
        newModel
    
    let view model dispatch =
        match model.PageType with
        | AddNew -> renderAddNewItemPage model dispatch
        | Edit -> renderEditItemPage model dispatch

