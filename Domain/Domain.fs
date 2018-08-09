module Domain

open Types

let up = (-1, 0)
let right = (0, 1)
let down = (1, 0)
let left = (0, -1)

let (|+|) (r1,c1) (r2,c2) = (r1+r2,c1+c2)

let mkLevel (arr:string list) : Board =
    let getGemFrom = function
        | 's' -> Some Spade
        | 'h' -> Some Heart
        | 'd' -> Some Diamond
        | 'c' -> Some Club
        | '.' -> None
        | _ -> failwith "inalid gem" 
    let rec processRow row col = function
        | ch::rest -> ((getGemFrom ch))::processRow row (col+1) rest
        | [] -> []
    let rec mkLevel' row = function
        | x::xs -> (processRow row 0 x)::mkLevel' (row+1) xs
        | [] -> []
    let stringToArray = List.map Seq.toList
    mkLevel' 0 (arr |> stringToArray)

let getCellAt (board:Board) (pos:Position) =
    let row,col = pos
    if (row >= 0 && row < List.length board
        && col >=0 && col < List.length board.[0]) then
        board.[row].[col]
    else
        None

let gemAt board pos =
    match getCellAt board pos with
    | None -> None
    | Some gem -> Some gem

let getNeighboringCells (board:Board) (pos:Position) =
    let offsets = [up;right;down;left]
    offsets |>
        List.map ((|+|) pos)
        |> List.filter (fun (r,c) -> 
            r >= 0 && r < List.length board &&
            c >= 0 && c < List.length board.[0])

let getMatchingNeighbors (board:Board) (pos:Position) =
    let gem = gemAt board pos
    getNeighboringCells board pos
    |> List.map (fun pos -> (pos, gemAt board pos))
    |> List.filter (fun (_,g) -> g <> None && g = gem)
    |> List.map (fun (p,_) -> p)

let anyMovesLeft (board: Board) =
    (board
    |> List.mapi (fun row cells ->
        cells
        |> List.mapi (fun col cell ->
           getMatchingNeighbors board (row,col)
           |> List.length
        )
        |> List.max
    )
    |> List.max) >= 1

let connectedCellsWithSameType (pos:Position) (board:Board) =
    let rec find gem pos result =
        match pos with
        | [] -> result
        | p::ps -> 
            let cells =
                p
                |> getNeighboringCells board
                |> List.filter (fun pos ->
                    gemAt board pos = gem)
                |> List.filter (fun pos ->
                    not (List.exists ((=) pos) (ps@result)))
            find gem (cells@ps) (p::result)
    find (gemAt board pos) [pos] []


let transposeBoard (board:Board) : Board =
    let h,w = List.length board, List.length board.[0]
    [for r in [0..w-1] ->
        [for c in [0..h-1] ->
            board.[c].[r]
        ]
    ]

let padLeft padding len lst =
    List.replicate (len-(List.length lst)) padding@lst

let padRight padding len lst =
    lst@List.replicate (len-(List.length lst)) padding

let dropGems (board:Board) : Board =
    let h = List.length board
    board
    |> transposeBoard
    |> List.map (fun row -> 
        row |> List.filter (fun cell -> not (cell = None))
            |> padLeft None h)
    |> transposeBoard

let isEmpty board =
    List.forall (fun rows -> List.forall (fun cell -> cell = None) rows) board

let collapseEmptyColumns (board:Board) : Board =
    if isEmpty board then
        board
    else
        let w = List.length board.[0]
        board
        |> transposeBoard
        |> List.filter(fun row ->
            not (List.forall (fun cell -> cell = None) row))
        |> transposeBoard
        |> List.map (padRight None w
)
let removeGemsAt locs (board:Board) : Board =
    board
    |> List.mapi (fun row cells ->
        cells |>
        List.mapi (fun col cell ->
            if List.contains (row,col) locs then
                None
            else
                cell))

let handleEvent event (game:Game) : Game =
    let score n = (n-1)*(n-1) 
    let board = game.Board
    match event with
    | CellClicked (row, col) ->
        let gem = (gemAt board (row, col))
        let connected = connectedCellsWithSameType (row, col) board
        let update =
            removeGemsAt connected
            >> dropGems
            >> collapseEmptyColumns
        if gem <> None && List.length connected >= 2 then
            { game with Board = update board; Score = game.Score + score (List.length connected) }
        else
            game

let mkRandomBoard rows cols seed =
    let r = new System.Random(seed)
    [for _ in [0..rows-1] ->
        [for _ in [0..cols-1] ->
            let i = r.Next(0,4)
            match i with
            | 0 -> Some Heart
            | 1 -> Some Spade
            | 2 -> Some Club
            | 3 -> Some Diamond
            | _ -> None
        ]
    ]