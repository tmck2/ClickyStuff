module Types

type Position = int * int

type Gem = Heart | Spade | Club | Diamond

type Cell = Gem option

type Row = Cell list

type Board = Row list
