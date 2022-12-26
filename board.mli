open Piece

(** The abstract type representing a chess board *)
type t

(** [position] is a specific sqaure on a chess board. 
    Example: White Rook starts at ('a',1). *)
type position = (char * int)

(** [find position board] is [Some Piece.t] if there is a piece bound to
    [position], and [None] if not. 
    Requires: [position] is a valid key in [board]. *)
val get_piece: position -> t -> Piece.t option

(** [remove_piece position board] mutates [board] to bound [None] to [position].
    If [position] is already bound to [None], then [board] is unchanged. 
    Requires: [position] is a valid key in [board]. *)
val remove_piece: position -> t -> unit 

(** [add_piece position piece board] mutates [board] to bind [position] to 
    [piece]. If [position] was already bound in [board], that binding is 
    replaced by the binding to [piece]. 
    Requires: [position] is a valid key in [board]. *)
val add_piece: position -> Piece.t option -> t -> unit

(** [move_piece initial_pos final_pos board] mutates [board] to move [piece] at
    [initial_pos] to [final_pos]. If [position] binds to [None], then [board]
    is unchanged. 
    Requires: [initials_pos] is a valid key in [board], and
    [final_pos] is a valid key in [board]. *)
val move_piece: position -> position -> t -> unit

(** [init_board] is a new table map with capacity 64 that will use the 
    default hash function to convert keys to integers. *)
val init_board: unit -> t

(** [bindings board] is an association list containing the same bindings
      as [board]. *)
val bindings: t -> (position * Piece.t option) list

(** [compare_position pos_1 pos_2] return whether or not [pos_1] is less than 
    greater [pos_2] by comparing their row and column. *)
val compare_position: position -> position -> int

(** [position_is_empty position board] is true if [position] doesn't contain any
    pieces. *)
val position_is_empty: position -> t -> bool

(** [positions board] is a list containing all position keys of [board]. *)
val positions: t -> position list

(** [black_positions board] is a list containing all position keys of [board] 
    that are bound to black pieces. *)
val black_positions: t -> position list

(** [white_positions board] is a list containing all position keys of [board] 
    that are bound to white pieces. *)
val white_positions: t -> position list

(** [board_evaluation board] outputs the board evaluation of [board] based on
    its pieces and their loactions. *)
val board_evaluation: t -> float

(** [board_evaluation board] outputs the board evaluation of [board] based only 
    on its pieces. *)
val greedy_board_evaluation: t -> float

(** [copy_board board] is a copy of [board]. *)
val copy_board: t -> t

(** [print_board board] prints out string representation of board. *)
val print_board: t -> unit