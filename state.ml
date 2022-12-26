open Board
open Move

type t = {
  current_board: Board.t;
  undo_stack: Board.t Stack.t;
  redo_stack: Board.t Stack.t
} 

type result = Legal of t | Illegal of string

let init_state () = {
  current_board = Board.init_board ();
  undo_stack = Stack.create ();
  redo_stack = Stack.create ()
}

let current_board st = st.current_board

let undo_stack st = st.undo_stack

(** [get_undo undo] is the top [board] on the [undo] stack. *)
let get_undo undo = Stack.pop undo

(** [add_undo st old_board] adds [old_board] to [st]'s [undo] stack. *)
let add_undo st old_board = Stack.push old_board (undo_stack st)

let redo_stack st = st.redo_stack

(** [get_redo redo] is the top [board] on the [redo] stack. *)
let get_redo redo = Stack.pop redo

(** [add_redo st old_board] adds [old_board] to [st]'s [redo] stack. *)
let add_redo st old_board = Stack.push old_board (redo_stack st)

let undo_move st = 
  try 
    let old_board = current_board st in 
    let current_board = get_undo (undo_stack st) in
    let () = add_redo st old_board in
    Legal { st with current_board = current_board }
  with Stack.Empty -> Illegal "No moves to undo"

let redo_move st = 
  try 
    let old_board = current_board st in 
    let current_board = get_redo (redo_stack st) in
    let () = add_undo st old_board in
    Legal { st with current_board = current_board }
  with Stack.Empty -> Illegal "No moves to redo"

let move initial_pos final_pos st =
  try 
    let old_board = Board.copy_board (current_board st) in
    Move.move initial_pos final_pos (current_board st);
    add_undo st old_board;
    Legal st
  with 
  | Move.EmptyPosition | Piece.NonePiece -> Illegal "No piece to move"
  | Move.IllegalMove -> Illegal "Illegal move"
  | _ -> Illegal "Something went wrong"

let result_parser result = 
  match result with 
  | Legal t -> t
  | Illegal str -> failwith str