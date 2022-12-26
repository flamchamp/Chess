open Move
open Board
open Piece

exception NoMovesDetected

(** [shuffle lst] outputs a randomly ordered version of [lst]. *)
let shuffle lst =
  Random.self_init ();
  let nd = List.map (fun c -> (Random.bits (), c)) lst in
  let sond = List.sort compare nd in
  List.map snd sond

(** [board_children color board] outputs a list of the possible boards after one
    move by the player whose color is [color]. *)
let board_children color board = 
  let board_after_move move = match move with 
    | (start_pos, end_pos) -> 
      let new_board = Board.copy_board board in 
      Move.move start_pos end_pos new_board; ((start_pos, end_pos), new_board)
  in (List.map board_after_move (Move.all_legal_moves color board)) 
     |> shuffle

(** [minimax_aux path board depth alpha beta maximizing_player greedy] executes
    the minimax algorithm, outputting the reversed version of the path to be 
    taken through the minimax tree and the board evaluation produced by this 
    path as a tuple in the form (path, eval). *)

let rec minimax_aux path board depth alpha beta maximizing_player =
  if depth = 0 || Move.game_over board 
  then (path, Board.board_evaluation board) 
  else if maximizing_player 
  then maximize path Float.min_float depth alpha beta
      (board_children White board)
  else minimize path Float.max_float depth alpha beta
      (board_children Black board)

(** [minimize path min_eval depth alpha beta children_lst] evaluates the 
    minimization layers of the minimax tree. Uses alpha-beta pruning. *)
and minimize path min_eval depth alpha beta = function
  | [] -> (path, min_eval)
  | h::t -> match h with 
    | (move, child) ->
      let path = move :: path  in
      let eval = 
        match minimax_aux path child (depth - 1) alpha beta true with
        | (_, eval) -> eval in
      let min_eval = if eval < min_eval 
        then eval 
        else min_eval in
      let beta = if beta < eval 
        then beta 
        else eval in
      if beta <= alpha
      then (path, min_eval) 
      else minimize path min_eval depth alpha beta t

(** [maximize path max_eval depth alpha beta children_lst] evaluates the 
    maximization layers of the minimax tree. Uses alpha-beta pruning. *)
and maximize path max_eval depth alpha beta = function
  | [] -> (path, max_eval)
  | h::t -> match h with 
    | (move, child) ->
      let path = move :: path in
      let eval = 
        match minimax_aux path child (depth - 1) alpha beta false with 
        | (_, eval) -> eval in
      let max_eval = if eval > max_eval 
        then eval 
        else max_eval in
      let alpha = if alpha > eval 
        then alpha 
        else eval in
      if alpha >= beta 
      then (path, max_eval) 
      else maximize path max_eval depth alpha beta t

(** [minimax board depth maximizing] extracts the move that should be
    taken next based on the minimax algorithm from minimax_aux. *)
let minimax board depth maximizing =
  match 
    minimax_aux [] board depth Float.min_float Float.max_float maximizing 
  with | (l, _) -> begin 
      match List.rev l with 
      | [] -> raise NoMovesDetected
      | h::_ -> h
    end

let move_minimax state depth maximizing =
  match minimax (State.current_board state) depth maximizing with 
  | (start_pos, end_pos) -> 
    State.move start_pos end_pos state 
    |> State.result_parser
