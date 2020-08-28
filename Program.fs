open System
open Domain
open FSharp.Collections
open TextCopy
open System.Text.Json
open System.IO
open FSharp.Data

let tryParseInt (str: string) =
    match str |> Int32.TryParse with
    | (false, _) -> None
    | (true, value) -> Some value

let promptForLang () =
    let allowedLanguages = ["en"; "fr"; "de"; "es"]
    printfn "Available languages: %s" (allowedLanguages |> String.concat "/")

    let mutable lang = None
    let mutable isFirstTime = true

    while Option.isNone lang = true do
        if isFirstTime = true then
            isFirstTime <- false
        else
            printfn "Invalid lang"

        printf "Choose a language for data: "
        let input = Console.ReadLine()

        lang <-
            if String.IsNullOrWhiteSpace input = true then
                Some "en"
            else if allowedLanguages |> List.contains input then
                Some input
            else    
                None

    lang.Value

let promptForApiKey () =
    let mutable apiKey = String.Empty
    let mutable isKeyValid = false
    let mutable isFirstTime = true

    while isKeyValid = false do
        if isFirstTime = true then
            isFirstTime <- false
        else
            printfn "Malformed key"

        printf "Account key: "
        apiKey <- Console.ReadLine()

        isKeyValid <- (String.IsNullOrWhiteSpace apiKey) = false

    apiKey

let promptForGuildSelection (guilds: Guild list) =
    let guilds = guilds |> List.mapi (fun i guild -> (i+1, guild))

    printfn "Choose a guild"
    for key, guild in guilds do
        printfn "%i - %s [%s]" key guild.Name guild.Tag


    let guildsMap = guilds |> Map.ofList
    
    let mutable selectedGuild = None
    let mutable isFirstTime = true

    while selectedGuild |> Option.isNone do
        if isFirstTime = true then
            isFirstTime <- false
        else
            printfn "Incorrect choice"

        printf "Your choice: "

        selectedGuild <-
            Console.ReadLine()
            |> tryParseInt
            |> Option.bind (fun key -> Map.tryFind key guildsMap)

    selectedGuild.Value

type GuildUpgradesSet = {
    //Done: Upgrade list
    Available: Upgrade list
    //Locked: Upgrade list
}

let getGuildUpgrades apiKey (guild: Guild) lang =
    let upgrades = Async.RunSynchronously (Api.getAllGuildUpgrades lang)
    let doneUpgradesIds = Async.RunSynchronously (Api.getDoneGuildUpgradesIds apiKey guild.Id)

    let upgradesUnderLevel, levelLockedUpgrades =
        upgrades |> List.partition (fun upgrade -> upgrade.RequiredLevel <= guild.Level)
        
    let doneUpgrades, pendingUpgrades =
        upgradesUnderLevel |> List.partition (fun upgrade -> doneUpgradesIds |> List.contains upgrade.Id)

    let availableUpgrades, prerequisitesLockedUpgrades =
        pendingUpgrades
        |> List.partition (fun upgrade ->
            upgrade.Prerequisites |> List.forall (fun upgradeId -> doneUpgradesIds |> List.contains upgradeId)
        )

    {
        //Done = doneUpgrades
        Available = availableUpgrades
        //Locked = [ levelLockedUpgrades; prerequisitesLockedUpgrades ] |> List.concat
    }

type ScreenState = List | Add | Remove

let getUpgradesQueue availables previousQueue =
    let drawList (queue: Upgrade list) (availables: Upgrade list) =
        let width, height = Console.WindowWidth, Console.WindowHeight
        let middle = width / 2

        let topPadding = 2
        let bottomPadding = 3
        let totalVPadding = topPadding + bottomPadding
        let maxUpgradesOnScreen = height - totalVPadding

        let maxTextLength = middle - 2 - 1

        Console.Clear()
        
        Console.SetCursorPosition(0,0)
        Console.Write("Upgrades queued")

        Console.SetCursorPosition(middle, 0)
        Console.Write("Available upgrades")

        let mutable line = topPadding
        for upgrade in queue.[..maxUpgradesOnScreen]  do
            Console.SetCursorPosition(0, line)
            printf "- %s" upgrade.Name.[..maxTextLength]

            line <- line + 1

        line <- topPadding
        for upgrade in availables.[..maxUpgradesOnScreen] do
            Console.SetCursorPosition(middle, line)
            printf "- %s" upgrade.Name.[..maxTextLength]

            line <- line + 1

        Console.SetCursorPosition(0, height - 1)
        printf "a - Add upgrade to queue  r - Remove upgrade from queue  d - Done"

    let drawAdd (availables: Map<int, Upgrade>) message =
        let width, height = Console.WindowWidth, Console.WindowHeight

        let topPadding = 2
        let bottomPadding = 3
        let totalVPadding = topPadding + bottomPadding
        let maxUpgradesOnScreen = height - totalVPadding

        let maxTextLength = width

        Console.Clear()
        
        Console.SetCursorPosition(0,0)
        Console.Write("Available upgrades")

        let mutable line = topPadding
        for key, upgrade in (availables |> Map.toList).[..maxUpgradesOnScreen] do
            Console.SetCursorPosition(0, line)
            Console.Write (sprintf "%i - %s" key upgrade.Name).[..maxTextLength]

            line <- line + 1

        if String.IsNullOrWhiteSpace message = false then 
            Console.SetCursorPosition(0, height - 2)
            Console.Write(message)

        Console.SetCursorPosition(0, height - 1)
        printf "Which upgrade do you want to add? "

    let drawRemove (queue: Map<int, Upgrade>) message =
        let width, height = Console.WindowWidth, Console.WindowHeight
    
        let topPadding = 2
        let bottomPadding = 3
        let totalVPadding = topPadding + bottomPadding
        let maxUpgradesOnScreen = height - totalVPadding
    
        let maxTextLength = width
    
        Console.Clear()
            
        Console.SetCursorPosition(0,0)
        Console.Write("Available queued")
    
        let mutable line = topPadding
        for key, upgrade in (queue |> Map.toList).[..maxUpgradesOnScreen] do
            Console.SetCursorPosition(0, line)
            Console.Write (sprintf "%i - %s" key upgrade.Name).[..maxTextLength]
    
            line <- line + 1
    
        if String.IsNullOrWhiteSpace message = false then 
            Console.SetCursorPosition(0, height - 2)
            Console.Write(message)
    
        Console.SetCursorPosition(0, height - 1)
        printf "Which upgrade do you want to remove? "

    let mutable state = List
    let mutable queue = previousQueue
    let mutable availables = availables
    let mutable exit = false
    let mutable message = ""

    while exit = false do
        match state with
        | List ->
            drawList queue availables
            message <- ""
        
            match Console.ReadKey(true).KeyChar with
            | 'a' -> state <- Add
            | 'r' -> state <- Remove
            | 'd' -> exit <- true
            | _ -> ()
        
        | Add ->
            let availablesMap = availables |> List.mapi (fun i u -> i+1, u) |> Map.ofList
            drawAdd availablesMap message
            message <- ""

            let selected =
                Console.ReadLine()
                |> tryParseInt
                |> Option.bind (fun key -> Map.tryFind key availablesMap)

            match selected with
            | Some upgrade ->
                queue <- upgrade :: queue
                availables <- availables |> List.filter (fun u -> u <> upgrade)
                state <- List
                
            | None -> message <- "Wrong number"

        | Remove ->
            let queueMap = queue |> List.mapi (fun i u -> i+1, u) |> Map.ofList
            drawRemove queueMap message
            message <- ""

            let selected =
                Console.ReadLine()
                |> tryParseInt
                |> Option.bind (fun key -> Map.tryFind key queueMap)

            match selected with
            | Some upgrade ->
                queue <- queue |> List.filter (fun u -> u <> upgrade)
                availables <- upgrade :: availables 
                state <- List
                
            | None -> message <- "Wrong number"
    
    queue

let makeReport upgrades treasury =
    let treasuryMap = treasury |> Map.ofList

    let itemsNeeded = 
        upgrades
        |> List.collect (fun u -> u.Items)
        |> List.fold (fun map (count, i) -> 
            match map |> Map.tryFind i.Id with
            | Some (_, oldCount) -> map |> Map.add i.Id (i, oldCount + count)
            | None -> map |> Map.add i.Id (i, count)
        ) Map.empty
        |> Map.toList
        |> List.map (fun (_, (item, count)) -> item, count)
        |> List.map (fun (item, total) ->
            let countInTreasury = treasuryMap |> Map.tryFind item.Id |> Option.defaultValue 0
            sprintf "- %s: %i/%i (%i)" item.Name countInTreasury total (Math.Max(total - countInTreasury, 0))
        )
        |> String.concat "\n"

    sprintf "List of ingredients for the following upgrades:\n\n%s" itemsNeeded

let copyToClipboard report = ClipboardService.SetText report

type Settings = {
    Version: string
    Language: string
    ApiKey: string
    GuildId: string
    Queue: int list
}

let saveSettings location settings =
    let options = new JsonSerializerOptions()
    options.WriteIndented <- true

    let serializedSettings = JsonSerializer.Serialize(settings, options)
    File.WriteAllText(location, serializedSettings)
    ()

// Source: https://fsharpforfunandprofit.com/posts/computation-expressions-intro/
type MaybeBuilder() =
    member this.Bind(x, f) =
        match x with
        | None -> None
        | Some a -> f a

    member this.Return(x) =
        Some x
   
let maybe = new MaybeBuilder()

let tryLoadSettings location =
    let tryGetString propertyName (json: JsonValue) = json.TryGetProperty propertyName |> Option.map (fun prop -> prop.AsString())
    let tryGetList propertyName (json: JsonValue) = json.TryGetProperty propertyName |> Option.map (fun prop -> [ for value in prop do yield value ])

    if File.Exists location then
        let json = File.ReadAllText location |> JsonValue.Parse

        maybe {
            let! version = json |> tryGetString "Version"
            let! language = json |> tryGetString "Language"
            let! apiKey = json |> tryGetString "ApiKey"
            let! guildId = json |> tryGetString "GuildId"
            let! rawQueue = json |> tryGetList "Queue"
            let queue = rawQueue |> List.map (fun value -> value.AsInteger())

            return {
                Version = version
                Language = language
                ApiKey = apiKey
                GuildId = guildId
                Queue = queue
            }
        }
    else
        None

[<EntryPoint; STAThread>]
let main argv =
    let saveLocation = "./settings.json"
    let settings = tryLoadSettings saveLocation

    let lang = settings |> Option.map (fun s -> s.Language) |> Option.defaultWith promptForLang
    let apiKey = settings |> Option.map (fun s -> s.ApiKey) |> Option.defaultWith promptForApiKey

    let guilds = Api.getAccountGuilds apiKey lang |> Async.RunSynchronously 

    let guild =
        settings |> Option.bind (fun s -> guilds |> List.tryFind (fun guild -> guild.Id = s.GuildId))
        |> Option.defaultWith (fun () -> promptForGuildSelection guilds)

    let upgrades = getGuildUpgrades apiKey guild lang

    let previousUpgradesQueued = settings |> Option.map (fun s -> s.Queue) |> Option.defaultValue []
    let previousQueue, availables = upgrades.Available |> List.partition (fun upgrade -> previousUpgradesQueued |> List.contains upgrade.Id)
    
    let queuedUpgrades = getUpgradesQueue availables previousQueue

    let treasury = Api.getGuildTreasury apiKey guild.Id |> Async.RunSynchronously

    let report = makeReport queuedUpgrades treasury
    Console.Clear()
    Console.WriteLine(report)
    copyToClipboard report

    Console.Write("\nThe text has been copied to the clipboard!\nPress Enter to quit.")

    saveSettings saveLocation {
        Version = "0.1.1"
        Language = lang
        ApiKey = apiKey
        GuildId = guild.Id
        Queue = queuedUpgrades |> List.map (fun u -> u.Id)
    }

    Console.ReadLine() |> ignore

    0
