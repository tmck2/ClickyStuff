module Types

type Position = int * int

type Event =
    | CellClicked of Position

type Gem = Heart | Spade | Club | Diamond

type Cell = Gem option

type Row = Cell list

type Board = Row list

type Game = {
    Board: Board
    Score: int
}
