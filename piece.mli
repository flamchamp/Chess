(** The abstract type representing any chess piece. *)
type t

(** [color] is the color of a chess piece. *)
type color = 
  | Black 
  | White

(** [grade] the type of piece of a chess piece.*)
type grade = 
  | Pawn 
  | Bishop 
  | Knight 
  | Rook 
  | Queen 
  | King

(** Raised when there is no piece at a position. *)
exception NonePiece

(** [init_piece grade color] is a [piece] with [color] and [grade]. Also, 
    intializes the [points] evaluation corresponding to the grade of the piece 
    and sets [has_moved] to false. *)
val init_piece: grade -> color -> t

(** [get_grade piece] is the [grade] of [piece]. *)
val get_grade: t -> grade

(** [get_color piece] is the [color] of [piece]. *)
val get_color: t -> color

(** [get_point piece] is the [points] of [piece]. *)
val get_point: t -> int

(** [get_num_moves piece] is the [moves] of [piece]. *)
val get_num_moves: t -> int

(** [set_num_moves piece] is a copy of [piece] with [moves] set to 
    number of moves from the [piece] last position, using the manhattan
    distance system. *)
val set_num_moves: t -> int -> t

(** [to_string_grade grade] is the string representation of [grade]. *)
val to_string_grade: grade -> string

(** [to_string_color color] is the string representation of [color]. *)
val to_string_color: color -> string

(** [to_string piece] is the string representation of [piece]. *)
val to_string: t -> string

(** [option_to_piece piece_op] decouples [Some piece] returning [piece]. 
    Raises [NonePiece] if [piece_op] is [None]. *)
val option_to_piece: t option -> t

(** [is_black_op piece_op] is [true] if [Some piece]'s color is [Black], 
    else [false] *)
val is_black_op: t option -> bool

(** [is_white_op piece_op] is [true] if [Some piece]'s color is [White], 
    else [false] *)
val is_white_op: t option -> bool

(** [multiplier position piece] multiplies the value of [piece] by its 
    [position]al strength*)
val multiplier: (char * int) -> t -> float
