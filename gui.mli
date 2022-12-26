open Piece
open Board

(** [print_start_page ()] prints the start page using its text file. *)
val print_start_page: unit -> unit

(** [print_instructions ()] prints the instrucstions. *)
val print_instructions: unit -> unit

(** [print_board board] prints the board [board]. *)
val print_board: Board.t -> unit
