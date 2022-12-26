module type PieceSig = sig
  type t
  type color = 
    | Black 
    | White
  type grade = 
    | Pawn 
    | Bishop 
    | Knight 
    | Rook 
    | Queen 
    | King
  exception NonePiece
  val init_piece: grade -> color -> t
  val get_grade: t -> grade
  val get_color: t -> color
  val get_point: t -> int
  val get_num_moves: t -> int
  val set_num_moves: t -> int -> t
  val to_string_grade: grade -> string
  val to_string_color: color -> string
  val to_string: t -> string
  val option_to_piece: t option -> t
  val is_black_op: t option -> bool
  val is_white_op: t option -> bool
  val multiplier: (char * int) -> t -> float
end

module PieceCheck : PieceSig = Piece

module type BoardSig = sig
  type t
  type position = (char * int)
  val get_piece: position -> t -> Piece.t option
  val remove_piece: position -> t -> unit 
  val add_piece: position -> Piece.t option -> t -> unit
  val move_piece: position -> position -> t -> unit
  val init_board: unit -> t
  val bindings: t -> (position * Piece.t option) list
  val compare_position: position -> position -> int
  val position_is_empty: position -> t -> bool
  val positions: t -> position list
  val black_positions: t -> position list
  val white_positions: t -> position list
  val board_evaluation: t -> float
  val copy_board: t -> t
  val print_board: t -> unit
end

module BoardCheck : BoardSig = Board

module type MoveSig = sig
  exception IllegalMove
  exception EmptyPosition
  exception ImpossibleBranchReached
  val legal_moves: Board.position -> Board.t -> Board.position list 
  val is_on_board: Board.position -> Board.t -> bool 
  val contains_piece: Board.position -> Board.t -> bool
  val is_legal: Board.position -> Board.position -> Board.t -> bool
  val move: Board.position -> Board.position -> Board.t -> unit 
  val game_over: Board.t -> bool
end

module MoveCheck : MoveSig = Move

module type CommandSig = sig 
  type object_phrase = string list
  type command = 
    | Move of ((char * int) * (char * int))
    | Instructions
    | Quit
    | Undo
    | Redo
    | Start of int
    | Moves of (char * int)  
    | Empty
    | Malformed
  val create_tuple : object_phrase -> (char * int) * (char * int)
  val create_pair : object_phrase -> (char * int)
  val parse : string -> command
end

module CommandCheck : CommandSig = Command

module type StateSig = sig 
  type t 
  type result = 
    | Legal of t 
    | Illegal of string
  val init_state: unit -> t
  val current_board: t -> Board.t
  val undo_stack: t -> Board.t Stack.t
  val redo_stack: t -> Board.t Stack.t
  val undo_move: t -> result
  val redo_move: t -> result
  val move: Board.position -> Board.position -> t -> result
  val result_parser: result -> t
end

module StateCheck : StateSig = State

module type GuiSig = sig 
  val print_start_page: unit -> unit
  val print_instructions: unit -> unit
  val print_board: Board.t -> unit
end

module GuiCheck : GuiSig = Gui

module type MinimaxSig = sig 
  val move_minimax: State.t -> int -> bool -> State.t
end

module MinimaxCheck : MinimaxSig = Minimax

module type AuthorSig = sig
  val hours_worked : int list
end

module AuthorCheck : AuthorSig = Author
