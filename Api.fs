module Api

open FSharp.Data
open FSharp.Data.JsonExtensions

open Domain

let private get url query lang =
    let query = [query; ["lang", lang]] |> List.concat
    Http.AsyncRequestString(
        url,
        query=query,
        httpMethod="GET"
    )

let private getAuth apiKey url query lang =
    let query = [query; ["lang", lang]] |> List.concat
    Http.AsyncRequestString(
        url,
        query=query,
        httpMethod="GET",
        headers=[HttpRequestHeaders.Authorization (sprintf "Bearer %s" apiKey)]
    )

let private getGuildData apiKey guildId =
    async {
        let! responseText = getAuth apiKey (sprintf "https://api.guildwars2.com/v2/guild/%s" guildId) [] "en"
        let json = JsonValue.Parse responseText

        return {
            Id = guildId
            Name = (json?name).AsString()
            Tag = (json?tag).AsString()
            Level = (json?level).AsInteger()
            Aetherium = (json?aetherium).AsInteger()
            Favor = (json?favor).AsInteger()
        }
    }

let getAccountGuilds apiKey lang =
    let parseResponse json =
        [ for guildId in (json |> JsonValue.Parse)?guild_leader do yield guildId.AsString() ]

    async {
        let! responseText = getAuth apiKey "https://api.guildwars2.com/v2/account" [] "en" 
        let guildIds = responseText |> parseResponse

        let! guilds = Async.Parallel [ 
            for guildId in guildIds do
                yield getGuildData apiKey guildId
        ]

        return Array.toList guilds
    }

let getItemsData (itemIds: int list) lang =
    let parseResponse json =
        [
            for itemJson in (json |> JsonValue.Parse) do 
                yield {
                    Id = (itemJson?id).AsInteger()
                    Name = (itemJson?name).AsString()
                }
        ]

    async {
        let batches = itemIds |> List.chunkBySize 200

        let! results =
            Async.Parallel (batches |> List.map (fun itemIds ->
                let idsParam = itemIds |> List.map string |> String.concat ","
                get "https://api.guildwars2.com/v2/items" ["ids", idsParam] lang
            ))

        return
            results
            |> Array.map parseResponse
            |> List.ofArray
            |> List.concat
    }

let getAllGuildUpgrades lang =
    let parseResponse json =
        let allowedUpgrades = ["AccumulatingCurrency";"BankBag";"Boost";"Claimable";"GuildHall";"GuildHallExpedition";"Hub";"Queue";"Unlock"]
        [
            for upgradeJson in json |> JsonValue.Parse do
                let costs = [
                    for costJson in (upgradeJson?costs) do
                        yield {|
                            Type = costJson.GetProperty("type").AsString()
                            Id = costJson.TryGetProperty("item_id") |> Option.map (fun json -> json.AsInteger()) |> Option.defaultValue 0
                            Name = costJson.TryGetProperty("name") |> Option.map (fun json -> json.AsString()) |> Option.defaultValue ""
                            Count = (costJson?count).AsInteger()
                        |}
                ]

                let aetherium =
                    match costs |> List.tryFind (fun cost -> cost.Name = "Aetherium") with
                    | Some cost -> cost.Count
                    | None -> 0

                let favor =
                    match costs |> List.tryFind (fun cost -> cost.Name = "Guild Favor") with
                    | Some cost -> cost.Count
                    | None -> 0

                let coins =
                    match costs |> List.tryFind (fun cost -> cost.Type = "Coins") with
                    | Some cost -> cost.Count
                    | None -> 0

                let prerequisites = [ for id in upgradeJson?prerequisites do yield id.AsInteger() ]

                let items = costs |> List.filter (fun cost -> cost.Type = "Item")

                yield {|
                    Id = (upgradeJson?id).AsInteger()
                    Name = (upgradeJson?name).AsString()
                    Description = (upgradeJson?description).AsString()
                    Type = upgradeJson.GetProperty("type").AsString()
                    RequiredLevel = (upgradeJson?required_level).AsInteger()
                    Aetherium = aetherium
                    Favor = favor
                    Coins = coins
                    Prerequisites = prerequisites
                    Items = items
                |}
        ] |> List.filter (fun upgrade -> List.contains upgrade.Type allowedUpgrades)
    async {
        let! responseText = get "https://api.guildwars2.com/v2/guild/upgrades" ["ids", "all"] lang
        let rawUpgrades = responseText |> parseResponse

        let itemIds = rawUpgrades |> List.collect (fun u -> u.Items) |> List.map (fun i -> i.Id) |> List.distinct
        let! items = getItemsData itemIds lang
        let itemsMap = items |> List.map (fun item -> (item.Id, item)) |> Map.ofList

        return [
            for upgrade in rawUpgrades do
            yield {
                Id = upgrade.Id
                Name = upgrade.Name
                Description = upgrade.Description
                RequiredLevel = upgrade.RequiredLevel
                Aetherium = upgrade.Aetherium
                Favor = upgrade.Favor
                Coins = upgrade.Coins
                Prerequisites = upgrade.Prerequisites
                Items = [
                    for rawItem in upgrade.Items do
                        let item = itemsMap |> Map.find rawItem.Id
                        yield rawItem.Count, item
                ]
            }
        ]
    }

let getDoneGuildUpgradesIds apiKey guildId =
    async {
        let! responseText = getAuth apiKey (sprintf "https://api.guildwars2.com/v2/guild/%s/upgrades" guildId) [] "en"
        let json = JsonValue.Parse responseText
        return [ for id in json do yield id.AsInteger()]
    }

let getGuildTreasury apiKey guildId =
    async {
        let! responseText = getAuth apiKey (sprintf "https://api.guildwars2.com/v2/guild/%s/treasury" guildId) [] "en"
        let json = JsonValue.Parse responseText
        return [
            for item in json do
                yield (item?item_id).AsInteger(), (item?count).AsInteger()
        ]
    }
