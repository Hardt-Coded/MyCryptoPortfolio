namespace MyCryptoPortfolio

open System
open Elmish.XamarinForms
open Elmish.XamarinForms.DynamicViews
open FSharp.Control
open Xamarin.Forms
open Services.SymbolService
open Newtonsoft.Json



module App =
    open Services
    
    type Model = {
        MainPageModel : MainPage.Model
        ItemPageModel: ItemPage.Model option
        SettingsPageModel: SettingsPage.Model
        MainColor:string
    }

    type Msg =
        | MainPageMsg of MainPage.Msg
        | ItemPageMsg of ItemPage.Msg
        | SettingsPageMsg of SettingsPage.Msg

        | OpenAddNewItemPage
        | OpenEditItemPage of PortfolioEntry        
        | CloseItemPage
        | SetMainColor of colorHex:string

    
    module Update =
        
        let setMainColor hexColor model =
            let newModel = {
                model with 
                    MainColor = hexColor
                    MainPageModel = {model.MainPageModel with MainColor = hexColor}
                    ItemPageModel = if model.ItemPageModel <> None then Some {model.ItemPageModel.Value with MainColor = hexColor} else None
                    SettingsPageModel = {model.SettingsPageModel with MainColor = hexColor}
            }
            
            // Store MainColor
            async {
                do! PortfolioService.MainColor.storeMainColorAsync hexColor                       
            } |> Async.Start

            newModel, Cmd.none


    module Views =
        
        let renderFavorite model entry dispatch = 
            View.ContentPage(
                    title= sprintf "%s" entry.Symbol,                            
                    content = View.StackLayout(
                            horizontalOptions = LayoutOptions.FillAndExpand,
                            verticalOptions = LayoutOptions.Center,
                            children = [
                                yield MainPage.Views.renderEntry model.MainColor entry dispatch
                            ]
                        
                        )
                )

    let init () = 

        let mainColor = PortfolioService.MainColor.getMainColor ()

        let mainModel, mainMsg = MainPage.init mainColor
        let settingsModel, settingsMsg = SettingsPage.init mainColor        

        {
            MainPageModel = mainModel
            ItemPageModel = None
            SettingsPageModel = settingsModel
            MainColor = mainColor
        }, Cmd.batch [Cmd.map MainPageMsg mainMsg; Cmd.map SettingsPageMsg settingsMsg]


    let update msg model =
        match msg with
        | MainPageMsg msg ->
            let m, cmd, externalMsg = MainPage.update msg model.MainPageModel

            let cmd2 =
                match externalMsg with
                | MainPage.ExternalMsg.Nope ->
                    Cmd.none
                | MainPage.ExternalMsg.OpenAddNewItemPage ->
                    Cmd.ofMsg OpenAddNewItemPage
                | MainPage.ExternalMsg.OpenEditItemPage item ->
                    Cmd.ofMsg (OpenEditItemPage item)                

            { model with MainPageModel = m }, Cmd.batch [ (Cmd.map MainPageMsg cmd); cmd2 ]

        | ItemPageMsg msg ->
            let m, cmd, externalMsg = ItemPage.update msg model.ItemPageModel.Value
            let cmd2 =
                match externalMsg with
                | ItemPage.ExternalMsg.Nope ->
                    Cmd.none
                | ItemPage.ExternalMsg.CloseItemPage ->
                    Cmd.ofMsg CloseItemPage
            
            { model with ItemPageModel = Some m }, Cmd.batch [ (Cmd.map ItemPageMsg cmd); cmd2 ]

        | SettingsPageMsg msg ->
            let m, cmd, externalMsg = SettingsPage.update msg model.SettingsPageModel
            let cmd2 =
                match externalMsg with
                | SettingsPage.ExternalMsg.Nope ->
                    Cmd.none
                | SettingsPage.ExternalMsg.SetMainColor colorHex ->
                    Cmd.ofMsg (SetMainColor colorHex)
            
            { model with SettingsPageModel = m }, Cmd.batch [ (Cmd.map SettingsPageMsg cmd); cmd2 ]

        | OpenAddNewItemPage ->
            let newItemPageModel, cmd = ItemPage.init ItemPage.AddNew model.MainColor
            let newModel = {model with ItemPageModel = Some newItemPageModel}
            newModel, (Cmd.map ItemPageMsg cmd)

        | OpenEditItemPage item ->
            let newItemPageModel, cmd = ItemPage.init ItemPage.Edit model.MainColor
            let itemPageModel = {newItemPageModel with Item = item}
            let newModel = {model with ItemPageModel = Some itemPageModel}
            newModel, (Cmd.map ItemPageMsg cmd)

        | CloseItemPage -> 
            // With every close of the ItemPage a new Item list is added to the MainPage model
            {
                model with 
                    ItemPageModel = None
                    MainPageModel = {model.MainPageModel with Items = PortfolioService.Items.getItems ()}
            }, Cmd.none
        | SetMainColor colorHex -> model |> Update.setMainColor colorHex
            
            
        
    let view (model: Model) dispatch =

        let mainPage = MainPage.view model.MainPageModel (MainPageMsg >> dispatch)

        let itemPage =
            match model.ItemPageModel with
            | None -> None
            | Some itemModel -> Some (ItemPage.view itemModel (ItemPageMsg >> dispatch))
        
        let settingsPage = SettingsPage.view model.SettingsPageModel (SettingsPageMsg >> dispatch)

        let favoritePage = 
            match model.MainPageModel.Items |> List.tryFind (fun i -> i.DisplayType = AsTab) with
            | None -> None
            | Some item -> Some (Views.renderFavorite model item (MainPageMsg >> dispatch))

        let pages = 
            match itemPage,favoritePage with
            | None,None -> [mainPage;settingsPage]
            | None,Some fp -> [mainPage;fp;settingsPage]
            | Some ip, _ -> [ip]
            

        View.TabbedPage(title = "MyCryptoPortfolio",            
            barBackgroundColor = Helpers.toColor model.MainColor,            
            children = pages
                )
            
    
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

   




