module Services

    open System.Net.Http    

    let httpClient = new HttpClient()
    exception Ex of string

    module RateService =
        open Newtonsoft.Json
        open System.Collections.Generic
        

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
                        | _ as e -> return raise (Ex("error getting rates: " + e.Message))

             }

    module SymbolService =
        open Newtonsoft.Json
        open System.Collections.Generic
        open MyCryptoPortfolio.FSharp
        
        
        
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
    





