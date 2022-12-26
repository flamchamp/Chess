open Piece 

type position = 
  (char * int)

type t = 
  (position, Piece.t option) Hashtbl.t

let get_piece position board = 
  try 
    Hashtbl.find board position 
  with _ -> None

let remove_piece position board = 
  Hashtbl.replace board position None

let add_piece position piece board = 
  Hashtbl.replace board position piece

let move_piece initial_pos final_pos board =
  try 
    let piece_op = get_piece initial_pos board in 
    let piece = Piece.option_to_piece piece_op in 
    let curr_moves = Piece.get_num_moves piece in
    let moved_piece = Piece.set_num_moves piece (curr_moves + 1) in
    let new_piece_op = Some moved_piece in 
    remove_piece initial_pos board; 
    add_piece final_pos new_piece_op board
  with Piece.NonePiece -> ()

(** [char_of_int] is the ASCII character of [col]. *)
let char_of_int col = Char.chr (col + 97)

let init_board () =
  let board = Hashtbl.create 64 in
  let piece_order = 
    [| Rook; Knight; Bishop; Queen; King; Bishop; Knight; Rook |] in
  for row = 1 to 8 do
    for col = 0 to 7 do
      let pos = (char_of_int(col), row) in
      let color = if row <= 2 
        then White 
        else Black in
      if row = 1 || row = 8 then
        add_piece pos (Some (Piece.init_piece piece_order.(col) color)) board
      else if row = 2 || row = 7 then
        add_piece pos (Some (Piece.init_piece Pawn color)) board
      else add_piece pos None board
    done
  done; 
  board

(** [compare_position pos_1 pos_2] is the compare function for positions that is
    used to sort them. 
    Ouputs 1 if
    - row of pos_1 > row of pos_2
    - row of pos_1 = row of pos_2 and column of pos_1 > column of pos_2 
      Outputs 0 if pos_1 and pos_2 have the same row and column.
      Outputs -1 otherwise. *)
let compare_position pos_1 pos_2 =
  let (col_1, row_1) = pos_1 in
  let (col_2, row_2) = pos_2 in
  if row_1 > row_2 then 1 
  else if row_1 < row_2 then -1 
  else if col_1 > col_2 then 1
  else if col_1 < col_2 then -1
  else 0

let position_is_empty position board =
  match Hashtbl.find board position with 
  | None -> true
  | _ -> false

let bindings board = 
  []
  |> Hashtbl.fold (fun position piece acc -> (position, piece) :: acc) board
  |> List.sort compare_position

let positions board = 
  []
  |> Hashtbl.fold (fun position _ acc -> position :: acc) board
  |> List.sort compare_position

let black_positions board = 
  []
  |> Hashtbl.fold (fun position piece acc -> 
      if Piece.is_black_op piece then position :: acc 
      else acc) board
  |> List.sort compare_position

let white_positions board = 
  []
  |> Hashtbl.fold (fun position piece acc -> 
      if Piece.is_white_op piece then position :: acc 
      else acc) board
  |> List.sort compare_position

let board_evaluation board = 
  let f acc = function
    | (_, None) -> acc
    | (position, piece_opt) -> let piece = piece_opt 
                                           |> Piece.option_to_piece in
      piece
      |> Piece.get_point 
      |> float_of_int
      |> ( *. ) (Piece.multiplier position piece)
      |> ( +. ) acc
  in List.fold_left f 0.0 (bindings board)

let greedy_board_evaluation board = 
  let f acc = function
    | (_, None) -> acc
    | (position, piece_opt) -> let piece = piece_opt 
                                           |> Piece.option_to_piece in
      piece
      |> Piece.get_point 
      |> float_of_int
      |> ( +. ) acc
  in List.fold_left f 0.0 (bindings board)

let copy_board board = Hashtbl.copy board

(** [reserve_concatenate] is a string of [str_2] ^ [str_1]. *)
let reserve_concatenate str_1 str_2 = str_2 ^ str_1

(** if playing as black then 9 - row -> row and col -> 7 - col *)
let print_board board =
  print_string "\n║                                                                                  ║\n";
  print_string "║                        ---------------------------------                         ║\n";
  for row = 1 to 8 do
    print_string ("║                       ");
    for col = 0 to 7 do
      let col = char_of_int col in
      try 
        board
        |> get_piece (col, 9 - row)
        |> Piece.option_to_piece
        |> Piece.to_string
        |> ( ^ ) " | "
        |> print_string
      with Piece.NonePiece -> print_string " |  "
    done;
    print_string (" | " ^ string_of_int (9 - row) ^ "                       ║\n");
    print_string "║                        ---------------------------------                         ║\n" 
  done;
  print_string "║                          ";
  for col = 0 to 7 do
    print_char (char_of_int col);
    print_string "   "
  done;
  print_string "                        ║\n";
  print_string "║                                                                                  ║\n";
