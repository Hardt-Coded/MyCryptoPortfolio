namespace MyCryptoPortfolio

module SettingsPage =

    open Xamarin.Forms
    open Elmish.XamarinForms
    open Elmish.XamarinForms.DynamicViews

    type Model = {            
            MainColor:string
            IsLoading:bool
        }

    type Msg = 
        | ChangeMainColor of colorHex:string

    type ExternalMsg =
        | Nope
        | SetMainColor of colorHex:string

    
    module Views =

        let private renderMainColorPicker model dispatch = 
            let rowCount = Consts.availableMainColors.Length / 5 |> int
            View.Grid(
                coldefs = [box "*";box "*";box "*";box "*";box "*"],
                rowdefs = [for _ in [0..rowCount - 1] do yield box "auto"],
                children = [
                    for x in [0..4] do
                        for y in [0..rowCount - 1] do
                            let currentIndex = 5 * y + x
                            if (currentIndex < Consts.availableMainColors.Length) then
                                yield View.Button(
                                        text=sprintf "%i" currentIndex,
                                        textColor = Color.White,
                                        backgroundColor = Consts.availableMainColors.[currentIndex],
                                        borderWidth = (if (Helpers.toColor model.MainColor) = Consts.availableMainColors.[currentIndex] then 2. else 0.),
                                        borderColor = Color.YellowGreen,
                                        command = fun () -> dispatch (ChangeMainColor (Helpers.fromColor Consts.availableMainColors.[currentIndex]))
                                        
                                    )
                                    .GridColumn(x)
                                    .GridRow(y)            
                ]
            )

        let renderSettingPage model dispatch =
            View.ContentPage(
                title = "Settings",
                content = View.Grid(
                    children = [
                        yield View.StackLayout(
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
                                    ]
                            )

                        if model.IsLoading then 
                            yield CommonViews.createBusyLayer()
                    ]
                )
            )

    let init mainColor = 
        let emptyModel =
            { 
              IsLoading = false              
              MainColor = mainColor              
            }
        emptyModel, Cmd.none
        
        

    let update msg model =         
        let newModel =
            match msg with            
            | ChangeMainColor colorHex -> model, Cmd.none, SetMainColor colorHex
        newModel

    let view = Views.renderSettingPage


