module Tests

open Xunit
open Types
open Domain

[<Fact>]
let ``handleEvent CellClicked`` () =
    let initial = mkLevel ["scd"
                           "ssd"
                           "ssd"]
    let expected = mkLevel [".d."
                            ".d."
                            "cd."]
    let actual = handleEvent (CellClicked (1,1)) initial
    Assert.Equal<Board>(expected, actual) 

[<Fact>]
let ``removeGemsAt works correctly`` () =
    let initial = mkLevel ["ss"
                           "ss"]
    let expected = mkLevel ["s."
                            ".s"]
    let actual = removeGemsAt [0,1; 1,0] initial
    Assert.Equal<Board>(expected, actual)
    
[<Fact>]
let ``collapseEmptyColumns works correctly`` () =
    let initial = mkLevel ["....."
                           "s.s.."]
    let expected = mkLevel ["....."
                            "ss..."]
    let actual = collapseEmptyColumns initial
    Assert.Equal<Board>(expected, actual)

[<Fact>]
let ``transposeBoard on a transposed board gives original`` () =
    let initial = mkLevel [".c"
                           "s."
                           "cc"]
    let expected = initial
    let actual = transposeBoard (transposeBoard initial)
    Assert.Equal<Board>(expected, actual)

[<Fact>]
let ``transposeBoard`` () =
    let initial = mkLevel [".sc"
                           "sc."
                           "ccc"]
    let expected = mkLevel [".sc"
                            "scc"
                            "c.c"]
    let actual = transposeBoard initial
    Assert.Equal<Board>(expected, actual)

[<Fact>]
let ``dropGems`` () =
    let initial = mkLevel [".ss.."
                           "ss..."
                           "....."]
    let expected = mkLevel ["....."
                            ".s..."
                            "sss.."]
    let actual = dropGems initial
    Assert.Equal<Board>(expected, actual)

[<Fact>]
let ``connectedCellsWithSameType returns exptected results`` () =
    let board = mkLevel ["scd"
                         "ssd"
                         "ssd"] 
    let expected = [0,0; 1,0; 1,1; 2,0; 2,1] |> List.sort
    let actual = connectedCellsWithSameType (1,0) board |> List.sort
    Assert.Equal<Position list>(expected, actual) 

[<Fact>]
let ``getNeighbors returns expected results`` () =
    let board = mkLevel ["dcd"
                         "ssd"
                         "hhd"] 
    let (expected: Position list) = [ (0,1); (1,2); (2,1); (1,0) ]
    let actual = getNeighboringCells board (1,1)
    Assert.Equal<Position list>(expected, actual)

[<Fact>]
let ``getCellAt returns None for out of bounds pos`` () =
    let board = mkLevel ["hh"
                         "hs"] 
    let (expected:Cell) = None
    let actual = getCellAt board (3,1)
    Assert.Equal<Cell>(expected, actual)

[<Fact>]
let ``getCellAt returns expected value`` () =
    let board = mkLevel ["hh"
                         "hs"] 
    let (expected:Cell) = Some Spade
    let actual = getCellAt board (1,1)
    Assert.Equal<Cell>(expected, actual)

[<Fact>]
let ``mkLevel with empty cell`` () =
    let (expected:Board) = [[Some Spade; None];
                            [Some Heart; Some Spade]]
    let actual = mkLevel ["s."
                          "hs"] 
    Assert.Equal<Board>(expected, actual)

[<Fact>]
let ``mkLevel multiple row multiple items`` () =
    let (expected:Board) = [[Some Spade; Some Heart];
                            [Some Heart; Some Spade]]
    let actual = mkLevel ["sh"
                          "hs"] 
    Assert.Equal<Board>(expected, actual)

[<Fact>]
let ``mkLevel one row multiple items`` () =
    let (expected:Board) = [[Some Spade; Some Heart]]
    let actual = mkLevel ["sh"] 
    Assert.Equal<Board>(expected, actual)

[<Fact>]
let ``mkLevel one spade`` () =
    let (expected:Board) = [[Some Spade]]
    let actual = mkLevel ["s"]
    Assert.Equal<Board>(expected, actual)

[<Fact>]
let ``mkLevel one heart`` () =
    let expected = [[Some Heart]]
    let actual = mkLevel ["h"]
    Assert.Equal<Board>(expected, actual)
