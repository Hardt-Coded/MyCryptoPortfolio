namespace MyCryptoPortfolio

module Services =

    open System.Net.Http    

    let httpClient = new HttpClient()
    

    module RateService =
        open Newtonsoft.Json
        open System.Collections.Generic
        open System
        

        type ExRate = { CurrencySymbol:string;Rate:decimal}
        type Rate = { CryptoSymbol:string;Rates:ExRate list}


        let rateResultToDictionary resultContent =
            try
                let rawDict = JsonConvert.DeserializeObject<Dictionary<string, Dictionary<string, decimal>>>(resultContent)
                let dict = 
                    rawDict
                    |> Seq.map ( fun x -> 
                        let cryptoSym = x.Key
                        let exRates = x.Value
                                        |> Seq.map (fun y -> {CurrencySymbol=y.Key;Rate=y.Value} )
                                        |> List.ofSeq
                        {CryptoSymbol = cryptoSym;Rates = exRates } 
                        )
                    |> List.ofSeq
                    |> Ok
                dict
            with
            | _ as e -> Error ("error converting result to dictionary: " + e.Message)
        
        let getRates currencySymbols outputSymbols = 
            async {                
                match currencySymbols with
                | [] ->                     
                    return raise (Ex "currency list must be filled to get rates")
                | _ ->                    
                    match outputSymbols with
                    | [] ->                         
                        return raise (Ex "output symbol list must be filled")
                    | _ -> 
                        try
                            let currencySymbolString = currencySymbols |> String.concat ","
                            let outputSymbolString = outputSymbols |> String.concat ","
                            let requestUrl = sprintf "https://min-api.cryptocompare.com/data/pricemulti?fsyms=%s&tsyms=%s" outputSymbolString currencySymbolString
                            let! result = httpClient.GetAsync(requestUrl) |> Async.AwaitTask
                            match result.IsSuccessStatusCode with
                            | false ->                                 
                                return raise (Ex ("error getting rates:" + result.StatusCode.ToString()))
                            | _ ->
                                let! content = result.Content.ReadAsStringAsync() |> Async.AwaitTask
                                let res = content |> rateResultToDictionary
                                match res with
                                | Ok v -> return v
                                | Error msg -> return raise (Ex(msg))
                                
                        with
                        | :? AggregateException as ag -> 
                            match ag.InnerExceptions |> Seq.toList with
                            | head::tail -> 
                                match head with
                                | :? HttpRequestException -> return raise (Ex("Error on connecting to the service. Check your network connection and try again."))
                                | _ as iex -> return raise iex
                            | [] -> return raise ag
                        | _ as e -> return raise e

             }

    module SymbolService =
        open Newtonsoft.Json
        open System.Collections.Generic
        open MyCryptoPortfolio
        
        
        
        type CoinType = {
            CoinName:string
        }
        type CurrencyTypeResponse = {
            Data:Dictionary<string,CoinType>
        }
        
        let defaultSymbols = 
            [
                ("BTC","Bitcoin")
                ("ETH","Ethereum")
                ("XRP","Ripple")
                ("BCH","Bitcoin Cash")
                ("EOS","EOS")
                ("XLM","Stellar")
                ("LTC","LiteCoin")
                ("TRX","TRON")
                ("IOT","IOTA")
            ] |> List.map (fun (sym,name) -> {Symbol=sym;Name=name})
            
        let convertRateResponseToDict resultContent =
            try
                let deserialized = JsonConvert.DeserializeObject<CurrencyTypeResponse>(resultContent)
                match deserialized.Data.Count with
                | 0 -> defaultSymbols |> Ok
                | _ ->
                    let currencyList = deserialized.Data
                                        |> Seq.map (fun x -> x.Key,x.Value.CoinName)
                                        |> List.ofSeq
                    
                    let currencyList' = currencyList
                                        |> List.sortBy (fun (key,value) -> key <> "",key <> "BTC",key<>"ETH",key<>"XRP", key<>"BCH",key<>"EOS",key<>"XLM",key<>"LTC",key<>"TRX",key<>"IOT",key)
                    currencyList' |> List.map (fun (sym,name) -> {Symbol=sym;Name=name}) |> Ok
            with
            | _ as e -> Error ("error converting rates! " + e.Message)


        let getAvailableCoins () = async {
            try
                let url = "https://min-api.cryptocompare.com/data/all/coinlist";
                let! result = httpClient.GetAsync(url) |> Async.AwaitTask
                match result.IsSuccessStatusCode with
                | false -> return defaultSymbols
                | _ ->
                    let! content = result.Content.ReadAsStringAsync() |> Async.AwaitTask
                    let dict = content |> convertRateResponseToDict 
                    match dict with
                    | Ok d -> return d
                    | Error _ -> return defaultSymbols
                
            with
            | _  -> return defaultSymbols

            }
    
    [<RequireQualifiedAccess>]
    module PortfolioService =
        open Xamarin.Forms
        open Newtonsoft.Json

        module Models =
            
            type MainViewStorageModel = {
                SelectedBaseCurrency:string
                PreviousBaseCurrency:string
            }
                with 
                    static member defaultValue =
                        {
                            SelectedBaseCurrency = "EUR"
                            PreviousBaseCurrency = "EUR"
                        } 

        module Items = 
            let private serializeList (list:PortfolioEntry list) = 
                JsonConvert.SerializeObject(list)
            let private deserializeList json =
                JsonConvert.DeserializeObject<PortfolioEntry list>(json)

            let getItems () =
                match Application.Current.Properties.TryGetValue Consts.entryStorageModelId with
                | true, (:? string as json) ->
                    try
                        json |> deserializeList
                    with
                    | _ as ex -> []
                | _ -> []
        
            let storeItemsAsync items =
                async {
                    let json = items |> serializeList
                    Application.Current.Properties.[Consts.entryStorageModelId] <- json
                    do! Application.Current.SavePropertiesAsync() |> Async.AwaitTask
                }

            let addItemsAsync item =
                async {
                    do! getItems()
                        |> List.append [item]
                        |> storeItemsAsync                
                }
            
            // This method only updates the Amout!
            let updateItemAsync item =
                async {
                    do! getItems()                                
                        |> List.map (fun i ->
                                        if i.Symbol <> item.Symbol then i
                                        else {i with 
                                                Amount = item.Amount                                            
                                            }
                                    )                    
                        |> storeItemsAsync                
                }

            let deleteItemAsync item =
                async {
                    do! getItems()                                
                        |> List.filter (fun i -> i.Symbol <> item.Symbol)
                        |> storeItemsAsync                
                }       

        module MainColor =

            let getMainColor () =
                match Application.Current.Properties.TryGetValue Consts.mainColorStorageModelId with
                | true, (:? string as hexColor) -> hexColor                
                | _ -> Helpers.fromColor Color.DarkViolet
        
            let storeMainColorAsync hexColor =
                async {                
                    Application.Current.Properties.[Consts.mainColorStorageModelId] <- hexColor
                    do! Application.Current.SavePropertiesAsync() |> Async.AwaitTask
                }

        module MainViewModel =
            open Models

            let getMainViewModel () =
                match Application.Current.Properties.TryGetValue Consts.mainviewStorageModelId with
                | true, (:? string as json) -> 
                    try
                        json |> JsonConvert.DeserializeObject<MainViewStorageModel>
                    with
                    | _ as ex ->  MainViewStorageModel.defaultValue                      
                | _ -> MainViewStorageModel.defaultValue  
        
            let storeMainViewModelAsync (model:MainViewStorageModel) =
                async {
                    let json = JsonConvert.SerializeObject(model)
                    Application.Current.Properties.[Consts.mainviewStorageModelId] <- json
                    do! Application.Current.SavePropertiesAsync() |> Async.AwaitTask
                }


