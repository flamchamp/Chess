(** The abstract type of values representing the game state. *)

type t 

(** The type representing the result of an attempted movement. *)
type result = Legal of t | Illegal of string

(** [init_state a] *)
val init_state: unit -> t

(** [current_board st] *)
val current_board: t -> Board.t

(** [undo st] *)
val undo_stack: t -> Board.t Stack.t

(** [redo st] *)
val redo_stack: t -> Board.t Stack.t

(** [undo_move st] *)
val undo_move: t -> result

(** [redo_move st] *)
val redo_move: t -> result

(** [move initial_pos final_pos st]
    Effects: none.  [move] is not permitted to do any printing. *)
val move: Board.position -> Board.position -> t -> result

(** [result_parser result] is the state of the result. 
    Raises [Failure str] if state change was invalid, [str] provides information
    why state change failed. *)
val result_parser: result -> t