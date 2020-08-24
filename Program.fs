// Learn more about F# at http://fsharp.org

open System
open System.Collections
open System.Collections.Generic
open System.IO
open System.Linq

let (++) a b = Path.Combine(a, b)

let folder = Environment.GetFolderPath(Environment.SpecialFolder.LocalApplicationData) ++ "osu!"

type OsuMode =
    | Standard
    | Taiko
    | CatchTheBeat
    | Mania

[<Flags>]
type OsuMods =
    | None = 0
    | NoFail = 1
    | Easy = 2
    | TouchDevice = 4
    | Hidden = 8
    | HardRock = 16
    | SuddenDeath = 32
    | DoubleTime = 64
    | Relax = 128
    | HalfTime = 256
    | NightCore = 512
    | Flashlight = 1024
    | Autoplay = 2048
    | SpunOut = 4096
    | Autopilot = 8192
    | Perfect = 16384
    | Key4 = 32768
    | Key5 = 65536
    | Key6 = 131072
    | Key7 = 262144
    | Key8 = 524288
    | KeyMod = 1015808
    | FadeIn = 1048576
    | Random = 2097152
    | Cinema = 4194304
    | TargetPractice = 8388608
    | Key9 = 16777216
    | Coop = 33554432
    | Key1 = 67108864
    | Key3 = 134217728
    | Key2 = 268435456

let hasMods (flags: OsuMods) (mods: OsuMods) =
    (LanguagePrimitives.EnumToValue flags) &&& (LanguagePrimitives.EnumToValue mods) <> 0

let accuracyThresholds = [|60.0; 70.0; 80.0; 88.0; 92.0; 95.0; 97.0; 98.0; 99.0; 99.99|]

type BinaryReader with

    member x.ReadOsuString() =
        match x.ReadByte() with
        | 0x00uy -> null
        | 0x0Buy -> x.ReadString()
        | _ -> failwith "Invalid osu! string"

    member x.ReadIntDoublePair() =
        if x.ReadByte() <> 0x8uy then failwith "invalid int-double pair"
        let int = x.ReadInt32()
        if x.ReadByte() <> 0xDuy then failwith "invalid int-double pair"
        let double = x.ReadDouble()

        (int, double)

    member x.Skip bytes = x.BaseStream.Seek(bytes, SeekOrigin.Current) |> ignore

type BinaryWriter with
    member x.WriteOsuString(string: string) =
        if string = null then x.Write(0uy)
        else
            x.Write(0xBuy)
            x.Write(string)

let readOsu () =

    use fs = File.OpenRead(folder ++ "osu!.db")
    use br = new BinaryReader(fs)

    let version = br.ReadInt32()
    br.ReadInt32() |> ignore // folder count
    br.ReadBoolean() |> ignore // account unlocked
    br.ReadInt64() |> ignore // date of unlock
    br.ReadOsuString() |> ignore // player name TODO check against scores
    let mapCount = br.ReadInt32()

    let playedMaps = HashSet<_>()

    let rec readBeatmap count =
        if count > 0 then
            if version < 20191106 then br.ReadInt32() |> ignore // entry size
            br.ReadOsuString() |> ignore // artist
            br.ReadOsuString() |> ignore // artist unicode
            br.ReadOsuString() |> ignore // song title
            br.ReadOsuString() |> ignore // song title unicode
            br.ReadOsuString() |> ignore // creator
            br.ReadOsuString() |> ignore // difficulty
            br.ReadOsuString() |> ignore // audio file
            let hash = br.ReadOsuString()
            br.ReadOsuString() |> ignore // .osu file
            let status = br.ReadByte()
            br.ReadInt16() |> ignore // hit circles
            br.ReadInt16() |> ignore // sliders
            br.ReadInt16() |> ignore // spinners
            br.ReadInt64() |> ignore // modified
            if version < 20140609 then
                br.ReadByte() |> ignore // ar
                br.ReadByte() |> ignore // cs
                br.ReadByte() |> ignore // hp
                br.ReadByte() |> ignore // od
            else
                br.ReadSingle() |> ignore // ar
                br.ReadSingle() |> ignore // cs
                br.ReadSingle() |> ignore // hp
                br.ReadSingle() |> ignore // od
            br.ReadDouble() |> ignore // sv


            let skipIntDoublePairs () =
                let rec loop count =
                    if count > 0 then
                        br.ReadIntDoublePair() |> ignore
                        loop (count - 1)
                loop (br.ReadInt32())

            skipIntDoublePairs ()
            skipIntDoublePairs ()
            skipIntDoublePairs ()
            skipIntDoublePairs ()

            br.ReadInt32() |> ignore // drain time, s
            br.ReadInt32() |> ignore // total time, ms
            br.ReadInt32() |> ignore // audio preview, ms

            let skipTimingPoints () =
                let rec loop count =
                    if count > 0 then
                        br.ReadDouble() |> ignore
                        br.ReadDouble() |> ignore
                        br.ReadBoolean() |> ignore
                        loop (count - 1)
                loop (br.ReadInt32())

            skipTimingPoints ()

            let mapId = br.ReadInt32()
            br.ReadInt32() |> ignore // set id
            br.ReadInt32() |> ignore // thread id

            br.ReadByte() |> ignore // standard grade
            br.ReadByte() |> ignore // taiko grade
            br.ReadByte() |> ignore // ctb grade
            br.ReadByte() |> ignore // mania grade

            br.ReadInt16() |> ignore // local offset
            br.ReadSingle() |> ignore // stack leniency

            br.ReadByte() |> ignore // mode

            br.ReadOsuString() |> ignore // song source
            br.ReadOsuString() |> ignore // song tags

            br.ReadInt16() |> ignore // online offset

            br.ReadOsuString() |> ignore // font

            let unplayed = br.ReadBoolean()

            br.ReadInt64() |> ignore // last played
            br.ReadBoolean() |> ignore // osz2?
            br.ReadOsuString() |> ignore // folder name
            br.ReadInt64() |> ignore // last checked

            br.ReadBoolean() |> ignore // ignore beatmap sound
            br.ReadBoolean() |> ignore // ignore beatmap skin
            br.ReadBoolean() |> ignore // disable storyboard
            br.ReadBoolean() |> ignore // disable video
            br.ReadBoolean() |> ignore // visual override

            if version < 20140609 then br.ReadInt16() |> ignore // unknown

            br.ReadInt32() |> ignore // last modification time?
            br.ReadByte() |> ignore // mania scroll speed

            if (not unplayed && status = 0x4uy) then playedMaps.Add(hash) |> ignore

            readBeatmap (count - 1)

    readBeatmap mapCount

    br.ReadInt32() |> ignore // user permissions
    playedMaps

let readScores () =
    let accuracies = Dictionary<_, _>()


    use fs = File.OpenRead(folder ++ "scores.db")
    use br = new BinaryReader(fs)

    br.ReadInt32() |> ignore // version

    let rec readMap mapCount =
        if mapCount > 0 then
            let mapHash = br.ReadOsuString()
            let scoreCount = br.ReadInt32()

            let rec readScore scoreCount =
                if scoreCount > 0 then
                    let mode =
                        match br.ReadByte() with
                        | 0uy -> Standard
                        | 1uy -> Taiko
                        | 2uy -> CatchTheBeat
                        | 3uy -> Mania
                        | unknown -> failwithf "Unknown game mode %i at position %x" unknown br.BaseStream.Position
                    br.ReadInt32() |> ignore // version
                    let hash = br.ReadOsuString()
                    if mapHash <> hash then failwithf "md5 mismatch at position %x" br.BaseStream.Position
                    br.ReadOsuString() |> ignore // player name
                    br.ReadOsuString() |> ignore // replay hash
                    let hit300 = br.ReadInt16()
                    let hit100 = br.ReadInt16()
                    let hit50 = br.ReadInt16()
                    br.ReadInt16() |> ignore // 激
                    br.ReadInt16() |> ignore // 勝
                    let hit0 = br.ReadInt16()
                    br.ReadInt32() |> ignore // score
                    br.ReadInt16() |> ignore // max combo
                    br.ReadBoolean() |> ignore // full combo
                    let (mods: OsuMods) = br.ReadInt32() |> LanguagePrimitives.EnumOfValue
                    br.ReadOsuString() |> ignore // empty
                    let timestamp = DateTime(br.ReadInt64())
                    br.ReadInt32() |> ignore // -1
                    br.ReadInt64() |> ignore // score id
                    if hasMods mods OsuMods.TargetPractice then br.ReadDouble() |> ignore

                    match mode with
                    | Standard ->
                        let accuracy =
                            let hit300 = float hit300
                            let hit100 = float hit100
                            let hit50 = float hit50
                            let hit0 = float hit0
                            let points = float (hit300 * 300.0 + hit100 * 100.0 + hit50 * 50.0)
                            let total = float ((hit300 + hit100 + hit50 + hit0) * 300.0)
                            if points > total then failwithf "%f > %f" points total
                            Math.Round(points / total * 100.0, 2)
                        match mods with
                        // TODO support other mods
                        | OsuMods.None ->
                            if not (accuracies.TryAdd(hash, accuracy)) then
                                accuracies.[hash] <- max accuracy accuracies.[hash]
                        | _ -> ()

                    | _ -> ()

                    readScore (scoreCount - 1)
            readScore scoreCount
            readMap (mapCount - 1)

    readMap (br.ReadInt32())
    accuracies

let readCollections () =
    use fs = File.OpenRead(folder ++ "collection.db")
    use br = new BinaryReader(fs)

    let version = br.ReadInt32()
    let collectionCount = br.ReadInt32()

    let collections = SortedDictionary<_, ISet<_>>()

    let rec readCollection count =
        if count > 0 then
            let name = br.ReadOsuString()
            let mapCount = br.ReadInt32()

            collections.Add(name, HashSet())

            let rec readHash count =
                if count > 0 then
                    collections.[name].Add(br.ReadOsuString()) |> ignore
                    readHash (count - 1)

            let hashes = readHash mapCount
            if name.StartsWith("*") then collections.Remove(name) |> ignore
            readCollection (count - 1)
    printfn "%i collections" collectionCount
    readCollection collectionCount

    (version, collections)

let writeCollections (version: int) (collections: IDictionary<_, _>) =

    use fs = File.Open(folder ++ "collection.db", FileMode.Truncate)
    use bw = new BinaryWriter(fs)

    bw.Write(version)

    bw.Write(collections.Count)

    for c in collections do
        let (key: string) = c.Key
        let (set: ISet<_>) = c.Value
        bw.WriteOsuString(key)
        bw.Write(set.Count)

        for (hash: string) in set do
            bw.WriteOsuString(hash)


[<EntryPoint>]
let main argv =
    let playedMaps = readOsu ()
    let accuracies = readScores ()

    for hash in playedMaps do
        accuracies.TryAdd(hash, 0.0) |> ignore

    let (version, collections) = readCollections ()
    for kvp in accuracies do
        let hash = kvp.Key
        let accuracy = kvp.Value


        let bracket =
            if accuracy <= 0.0 then "×"
            elif accuracy >= 100.0 then "SS"
            else
                let threshold = Array.find (fun x -> accuracy <= x) accuracyThresholds
                sprintf "≤%.2f%%" threshold


        let collectionName = sprintf ("* Accuracy: NM %s") bracket //TODO: other mods

        collections.TryAdd(collectionName, HashSet()) |> ignore

        collections.[collectionName].Add(hash) |> ignore

    writeCollections version collections
    0
