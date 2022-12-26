open Board
open Piece

exception IllegalMove
exception EmptyPosition
exception ImpossibleBranchReached

(** ------------------------ General move functions ------------------------ *)

let is_on_board position board =
  List.mem position (Board.positions board)

let contains_piece position board =
  match Board.get_piece position board with
  | Some piece -> true
  | None -> false

(** [char_add c i] increments the char [c] by [i] letters. *)
let char_add c i = 
  Char.chr((Char.code c) + i)

(** [comb_2_lists] takes in 2 lists, and [comb_4_lists] takes in 4 lists.
    Both functions output a list in which all of their inputs are appended 
    together, in the order in which they are passed in. *)
let comb_2_lists a b = 
  List.append a b

let comb_4_lists a b c d = 
  comb_2_lists (comb_2_lists a b) (comb_2_lists c d)

(** [position_offset position board num_forward num_right] takes in a position
    [position] on a board [board] and outputs the the position whose 
    y-coordinate is incremenred by [num_forward] and whose x-coordinate is 
    incremented by [num_right] *)
let position_offset position board num_forward num_right =
  match position with
  | (c, i) -> (char_add c num_right, i + num_forward)

(** [is_blocked position board num_forward num_right] outpurs true if the 
    path between the position [position] on the board [board] and the position
    [position_offset position board num_forward num_right] is blocked by 
    another piece and false otherwise. *)
let is_blocked position board num_forward num_right =
  if contains_piece (position_offset position board num_forward num_right) board 
  then true 
  else false

(** [position_color position board] returns the color of the piece in the 
    position [position] on the board [board]. *)
let position_color position board = 
  Piece.get_color (Board.get_piece position board |> Piece.option_to_piece)

(** [filter_out_landing_on_self position board pos_lst] outputs list that is
    the same as [pos_list] with the pieces in the positions in [pos_list] that 
    share the same color as the piece in [position] filtered out. *)
let filter_out_landing_on_self position board pos_lst =
  let f pos1 pos2 = if contains_piece pos2 board 
    then begin
      if position_color pos1 board = position_color pos2 board 
      then false 
      else true
    end else true in
  List.filter (f position) pos_lst

(** ------------------------ Pawn move functions ------------------------ *)

(** [check_front i position board] outputs a list that contains the position
    in front of the piece in [position] and the postion two ahead of the piece 
    if a pawn could more to them. 
    Depends on whether the pawn has moved before. 
    [i] is 1 if the pawn is white and -1 if it is black, specifying the 
    direction that "front" is in. *)
let check_front i position board =
  if is_blocked position board (1 * i) 0
  then [] 
  else begin
    (position_offset position board (1 * i) 0) ::
    if (Piece.get_num_moves
          (Board.get_piece position board 
           |> Piece.option_to_piece) > 0)
    || is_blocked position board (2 * i) 0
    then []
    else [(position_offset position board (2 * i) 0)]
  end 

(** [check_diagonals i position board] checks if a pawn in [postion] can kill
    a piece in one of its diagonals. Outputs a list of the diagonal positions to
    which it can move. 
    [i] is 1 if the pawn is white and -1 if it is black, specifying the 
    direction that "front" is in. *)
let check_diagonals i position board =
  let left_diagonal = position_offset position board (1 * i) (-1) in
  let right_diagonal = position_offset position board (1 * i) 1 in
  let lst = if not (is_on_board left_diagonal board) 
    then [] 
    else begin
      if not (is_blocked position board (1 * i) (-1))
      then [] 
      else [left_diagonal] 
    end in
  if not (is_on_board right_diagonal board) 
  then lst 
  else begin 
    if not (is_blocked position board (1 * i) 1) 
    then lst 
    else (right_diagonal) :: lst
  end

let is_empassant_helper position board int_direction = 
  if (contains_piece (position_offset position board 0 int_direction) board) 
  then 
    let piece_right_grade = 
      Board.get_piece (position_offset position board 0 int_direction) board 
      |> Piece.option_to_piece 
      |> Piece.get_grade in
    let piece_right = 
      Board.get_piece (position_offset position board 0 int_direction) board 
      |> Piece.option_to_piece in
    if (piece_right_grade = Pawn && Piece.get_num_moves piece_right = 1) 
    then true 
    else false 
  else false 

let is_enpassant position board color =
  let rank = match position with |(c, i) -> i in
  if (rank = 5 && color = White || rank = 4 && color = Black ) 
  then if (is_empassant_helper position board (-1) || 
           is_empassant_helper position board 1) 
    then true 
    else false 
  else false 

let enpassant_helper position board right_pos left_pos int_color =
  if (contains_piece right_pos board && contains_piece left_pos board) 
  then [position_offset (right_pos) board int_color 0 ; 
        position_offset (left_pos) board int_color 0] 
  else if (contains_piece right_pos board) 
  then [position_offset (right_pos) board int_color 0]
  else if (contains_piece left_pos board) 
  then [position_offset (left_pos) board int_color 0]
  else failwith "Illegal Enpassant"

let enpassant position board color = 
  let piece_right_pos = position_offset position board 0 1 in 
  let piece_left_pos = position_offset position board 0 (-1) in
  if color = White then
    enpassant_helper position board piece_right_pos piece_left_pos 1
  else enpassant_helper position board piece_right_pos piece_left_pos (-1)

(** [legal_pawn_moves position board] produces a list of the legal moves for 
    a pawn in [position] to move to on the board [board]. 
    Precondition: [position] contains a pawn. *)
let legal_pawn_moves position board =
  let i = if position_color position board = White 
    then 1 
    else -1 in
  let color =  position_color position board in
  if is_enpassant position board color 
  then comb_2_lists (comb_2_lists (check_front i position board) 
                       (check_diagonals i position board)) 
      (enpassant position board color)
  else comb_2_lists (check_front i position board) 
      (check_diagonals i position board)

(** ------------------ Bishop/Rook/Queen move functions ------------------ *)

(** The follwing 3 functions are used to extract information from postions 
    and to perform operations on columns, which are specificed by chars. *)
let int_of_char c = 
  Char.code c - 97

let row position = 
  match position with 
  | (c,i) -> int_of_char c

let col position = 
  match position with 
  | (c,i) -> i

(** [diagonals position board num_forward num_right] produces a list of places
    where the piece in [postion] can move to on the diagonal specified by 
    [num_forward] and [num_right] until the first time it is blocked. Used to 
    determine the leal moves of bishops, rooks, and queens. *)
let rec diagonals position board num_forward num_right =
  let next_pos = position_offset position board num_forward num_right in 
  if (is_on_board next_pos board) then begin
    if is_blocked position board num_forward num_right
    then [next_pos] 
    else next_pos :: diagonals next_pos board num_forward num_right
  end
  else []

(** The following 4 functions generate the legal moves from [position] on its 
    4 diagonals. "fr" denotes front-right, "br" denotes back-right, "fl" deontes 
    front-left, "bl" denotes back-left. *)
let diagonal_fr position board = diagonals position board 1 1
let diagonal_br position board = diagonals position board (-1) 1
let diagonal_fl position board = diagonals position board 1 (-1)
let diagonal_bl position board = diagonals position board (-1) (-1)

(** [legal_bishop_moves position board] outputs the list of possible legal moves 
    for a bishop in [position]. Precondition: [position] contains a bishop. *)
let legal_bishop_moves position board =
  comb_4_lists 
    (diagonal_fr position board) 
    (diagonal_bl position board) 
    (diagonal_fl position board) 
    (diagonal_br position board)

(** The following 4 functions are the same as the above diagonal functions but
    for rows and columns rather than diagonals. They are used to produce the 
    legal moves of rooks and queens. *)
let horizontal_r position board = diagonals position board 0 1
let horizontal_l position board = diagonals position board 0 (-1)
let vertical_f position board = diagonals position board 1 0
let vertical_b position board = diagonals position board (-1) 0

(** [legal_rook_moves position board] outputs the list of possible legal moves 
    for a rook in [position]. Precondition: [position] contains a rook. *)
let legal_rook_moves position board =
  comb_4_lists 
    (horizontal_r position board) 
    (horizontal_l position board) 
    (vertical_f position board) 
    (vertical_b position board)

(** [legal_queen_moves position board] outputs the list of possible legal moves 
    for a queen in [position]. Precondition: [position] contains a queen. *)
let legal_queen_moves position board =
  comb_2_lists 
    (legal_bishop_moves position board) 
    (legal_rook_moves position board)

(** ------------------------ Knight move functions ------------------------ *)

(** [filter func board] filters out positions on a board based on the predicate
    function [func]. *)
let filter func board = List.filter func (Board.positions board) 

(** [legal_knight_moves position board] outputs the list of possible legal moves 
    for a knight in [position]. Precondition: [position] contains a knight. *)
let legal_knight_moves position board =
  let f pos = if (abs (row position - row pos) = 1 &&
                  abs (col position - col pos) = 2) || 
                 (abs (row position - row pos) = 2 &&
                  abs (col position - col pos) = 1) 
    then true else false in 
  List.sort_uniq Board.compare_position (filter f board)

(** -------------------------- Castling functions -------------------------- *)

(** [king_has_moved color board] returns true if the king of the color [color]
    has moved yet in the game and false otherwise. *)
let king_has_moved color board = 
  let position = if color = White 
    then ('e',1) 
    else ('e',8) in
  if (Board.position_is_empty position board) then true 
  else Board.get_piece position board |> Piece.option_to_piece 
       |> Piece.get_num_moves > 0 

(** [rook_has_moved color right board] returns true if the rook of the color 
    [color] on the right side if [right] = true and left side if [right] = false
    has moved yet in the game and false otherwise. *)
let rook_has_moved color right board =
  let position = if color = White 
    then if right 
      then ('h',1) 
      else ('a',1)
    else if right = true 
    then ('h',8) 
    else ('a',8) in
  if (Board.position_is_empty position board) 
  then true 
  else Board.get_piece position board |> Piece.option_to_piece 
       |> Piece.get_num_moves > 0 

(** [castle_path color right] returns the list of positions in the path of the 
    king of color [color] that are in the path to castle on one of its sides. 
    The side is right if [right] = true and left if [right] = false. *)
let castle_path color right =
  match color, right with
  | White, true -> [('e',1);('f',1);('g',1)]
  | White, false -> [('e',1);('d',1);('c',1);('b',1)]
  | Black, true -> [('e',8);('f',8);('g',8)]
  | Black, false -> [('e',8);('d',8);('c',8);('b',8)]

(** [castle_path_is_empty color right board] returns true if all positions but
    the first in the castle path of the [color] on the side specified by [right]
    is empty and false otherwise. *)
let castle_path_is_empty color right board =
  let position_is_empty acc position = 
    (Board.position_is_empty position board) && acc
  in match castle_path color right with 
  | h::t -> List.fold_left position_is_empty true t
  | _ -> raise ImpossibleBranchReached

(** [remove_last_elt lst] returns the list [lst] but with the last element 
    removed. *)
let rec remove_last_elt = function
  | [] -> []
  | h::[] -> [h]
  | h::t -> h :: (remove_last_elt t)

(** [can_castle_white_left board] returns true if the white king can castle on 
    the left. *)
let can_castle_white_left board = 
  not (king_has_moved White board) && 
  not (rook_has_moved White false board) &&
  castle_path_is_empty White false board

(** [can_castle_white_right board] returns true if the white king can castle on 
    the right. *)
let can_castle_white_right board = 
  not (king_has_moved White board) && 
  not (rook_has_moved White true board) &&
  castle_path_is_empty White true board

(** [can_castle_black_left board] returns true if the black king can castle on 
    the left. *)
let can_castle_black_left board = 
  not (king_has_moved Black board) && 
  not (rook_has_moved Black false board) &&
  castle_path_is_empty Black false board

(** [can_castle_black_right board] returns true if the black king can castle on 
    the left. *)
let can_castle_black_right board = 
  not (king_has_moved Black board) && 
  not (rook_has_moved Black true board) &&
  castle_path_is_empty Black true board

(** [is_castle start_pos end_pos board] returns true if the move from 
    [start_pos] to [end_pos] is a castling move. *)
let is_castle start_pos end_pos board =
  match start_pos, end_pos with 
  | ('e',1), ('g',1) -> if can_castle_white_right board 
    then true else false
  | ('e',1), ('c',1) -> if can_castle_white_left board 
    then true else false
  | ('e',8), ('g',8) -> if can_castle_black_right board 
    then true else false
  | ('e',8), ('c',8) -> if can_castle_black_left board 
    then true else false
  | _ -> false

(** [castle start_pos end_pos board] performs the castling move corresponding 
    to the move of the king from [start_pos] to [end_pos].
    Precondition: for any [start_pos], [end_pos] [board] in [castle start_pos 
    end_pos board], [is_castle start_pos end_pos board] is true *)
let castle start_pos end_pos board =
  match start_pos, end_pos with
  | ('e',1), ('g',1) -> 
    Board.move_piece ('e',1) ('g',1) board;
    Board.move_piece ('h',1) ('f',1) board
  | ('e',1), ('c',1) -> 
    Board.move_piece ('e',1) ('c',1) board;
    Board.move_piece ('a',1) ('d',1) board
  | ('e',8), ('g',8) -> 
    Board.move_piece ('e',8) ('g',8) board;
    Board.move_piece ('h',8) ('f',8) board
  | ('e',8), ('c',8) -> 
    Board.move_piece ('e',8) ('c',8) board;
    Board.move_piece ('a',8) ('d',8) board
  | _ -> raise ImpossibleBranchReached

(** -------------------------- King move function -------------------------- *)

(** [legal_king_moves position board] outputs the list of possible legal moves 
    for a king in [position], including castling moves. 
    Precondition: [position] contains a king. *)
let legal_king_moves position board =
  let f pos = if abs (row position - row pos) <= 1 &&
                 abs (col position - col pos) <= 1
    then true else false in 
  let moves = filter f board in
  match Board.get_piece position board |> Piece.option_to_piece 
        |> Piece.get_color with
  | White -> 
    let moves = if can_castle_white_left board 
      then ('c',1) :: moves 
      else moves
    in if can_castle_white_right board 
    then ('g',1) :: moves 
    else moves
  | Black -> 
    let moves = if can_castle_black_left board 
      then ('c',8) :: moves 
      else moves 
    in if can_castle_black_right board 
    then ('g',8) :: moves 
    else moves

(** -------------------- Check/checkmate logic functions -------------------- *)

(** [king_pos color board] outputs the positon of the king of [color]. *)
let king_pos color board =
  let my_positions = match color with
    | Black -> Board.black_positions board
    | White -> Board.white_positions board in
  let f position = match Board.get_piece position board with
    | Some piece when Piece.get_grade piece = King -> true
    | _ -> false in
  match List.filter f my_positions with
  | h::[] -> h
  | _ -> raise ImpossibleBranchReached

(** [check_legal_moves position board] outputs the legal_moves of the piece in
    [position] based on the type of piece it is and the board state. It does not 
    filter out putting oneself in check as this is the function called by the 
    check-related functions and doing so would result in infinite recursion. 
    If [position] is empty, raises the EmptyPosition exception. *)
let check_legal_moves position board =
  if contains_piece position board then begin
    let piece = Board.get_piece position board 
                |> Piece.option_to_piece in
    board |> (match (Piece.get_grade piece) with
        | Pawn -> legal_pawn_moves position
        | Bishop -> legal_bishop_moves position 
        | Knight -> legal_knight_moves position
        | Rook -> legal_rook_moves position
        | Queen -> legal_queen_moves position
        | King -> legal_king_moves position)
    |> filter_out_landing_on_self position board
  end 
  else raise EmptyPosition

(** [is_in_check color board] returns true if the player whose color is [color]
    if in check. *)
let is_in_check color board =
  let oponent_positions = match color with 
    | Black -> Board.white_positions board 
    | White -> Board.black_positions board in
  let oponent_moves = 
    List.fold_left (fun a pos -> (check_legal_moves pos board) @ a) [] 
      oponent_positions
  in List.mem (king_pos color board) oponent_moves

(** -------------------- General legal moves functions -------------------- *)

(** [filter_putting_self_in_check color board current_position next_positions]
    outputs [next_positions] but with moves from [position] to elements of 
    [next_positions] that would put the player into check filtered out. *)
let filter_putting_self_in_check color board current_position next_positions = 
  let f position = 
    let new_board = Board.copy_board board in
    let () = Board.move_piece current_position position new_board in
    if is_in_check color new_board 
    then false 
    else true in
  List.filter f next_positions 

let legal_moves position board =
  let piece = Board.get_piece position board 
              |> Piece.option_to_piece in
  board 
  |> check_legal_moves position 
  |> filter_putting_self_in_check (Piece.get_color piece) board position

(** ------------------------- Primary move functions ------------------------ *)

(** [equals pos1 pos2] is true if pos1 = pos2 and false otherwise. *)
let equals pos1 pos2 =
  if pos1 = pos2 
  then true 
  else false 

let is_legal start_pos end_pos board =
  if Array.exists (equals end_pos) (Array.of_list (legal_moves start_pos board))
  then true 
  else false 

let all_legal_moves color board =
  let my_positions = match color with
    | White -> Board.white_positions board
    | Black -> Board.black_positions board in
  let f acc position = (List.map (fun end_pos -> (position, end_pos)) 
                          (legal_moves position board)) 
                       |> List.fold_left (fun acc elt -> elt :: acc) acc 
  in List.fold_left f [] my_positions

(** The following 2 functions extract all of the legal moves of their respective
    color. *)
let all_legal_white_moves = all_legal_moves White
let all_legal_black_moves = all_legal_moves Black

let move start_pos end_pos board =
  if is_legal start_pos end_pos board 
  then if is_castle start_pos end_pos board 
    then castle start_pos end_pos board
    else Board.move_piece start_pos end_pos board
  else raise IllegalMove

(** [is_in_checkmate color board] is true if the player whose color is [color]
    is in checkmate given the board state [board]. This is determined by 
    checking if the player is currently in check and if none of its moves 
    change it to not being in check. *)
let is_in_checkmate color board =
  let moves = all_legal_moves color board in
  let f move = match move with
    | (start_pos, end_pos) -> begin
        match filter_putting_self_in_check color board start_pos [end_pos] with
        | [] -> false
        | _ -> true end in
  if is_in_check color board
  then match List.filter f moves with
    | [] -> true
    | _ -> false
  else false

let game_over board =
  is_in_checkmate White board || is_in_checkmate Black board