open Board
open Move
open Command
open Minimax

(** The [depth] refence is set when the player inputs a difficulty at the start
    of a game and is passed into the call to the minimax algorithm every it is 
    executed. *)

let depth = ref 0

(** The following print functions were factored out of the code for clarity. 
    The update and notify the player mistakes they made, what the computer is 
    exeecuting, and the instructions for the game. *)

let print_instructions () = 
  print_endline "\nEnter \"move <position1> <position2>\", where \
                 <position1> and <position2> are positions on the board \
                 such as a1 or h6, or \"quit\" to quit the game."

let print_enter_command () = 
  print_string "\nEnter a command\n> "

let print_invalid_move () = 
  ANSITerminal.(print_string [red] "The move you entered was invalid. \
                                    Please try again.\n")

let print_invalid_command () = 
  ANSITerminal.(print_string [red] "The command you entered was invalid. \
                                    Please try again.\n")

let print_unrecognized_difficulty () =
  print_endline "Difficulty not recognized. Enter one of the following: \
                 \"easy\", \"medium\", \"hard\", or \"expert\"."

let print_generating_optimal_move () =
  print_endline "\n\n...\n\nThe computer is using the minimax algorithm to \
                 choose the best possible move given the difficulty \
                 selected.\n\n...\n\n"

let print_generating_random_move () =
  print_endline "\n\n...\n\nThe computer is choosing a random black piece to \
                 move to a random legal position\n\n...\n\n"

(** [game_over ()] notifies the player that the game is over and then 
    terminates the running of main. *)
let game_over () = 
  ANSITerminal.(print_string [red] "\nCheckmate! Game Over!\n\n"); exit 0

(** [random_list_entry lst] takes in a list [lst] and outputs a random entry of 
    it. *)
let random_list_entry lst =
  Random.self_init ();
  let arr = Array.of_list lst in
  arr.(Random.int64 (Array.length arr |> Int64.of_int) |> Int64.to_int)

(** [generate_random_black_move board] generates a random move for the computer
    to perform and proceeds to execute this move. *)
let rec generate_random_black_move state = 
  let board = State.current_board state in 
  let start_pos = random_list_entry (Board.black_positions board) in
  let legal_moves = Move.legal_moves start_pos board in
  if List.length legal_moves = 0 
  then generate_random_black_move state
  else let end_pos = random_list_entry legal_moves in
    State.move start_pos end_pos state |> State.result_parser

(** [try_to_move board] moves one of the computers pieces based on the depth
    reference. If the depth is 0, it randomly generates a move, while if the 
    depth is anything else, it uses the minimix algorithm, inputting the depth 
    in as the tree's depth, to generate a move. *)
let try_to_move state = 
  let state' = match !depth with 
    | 0 -> begin
        print_generating_random_move (); 
        generate_random_black_move state
      end 
    | d -> begin
        print_generating_optimal_move ();
        Minimax.move_minimax state d false 
      end in
  let board = State.current_board state' in 
  Gui.print_board board

(** [print_list lsr str] print out [lst] folded on [str] *)
let rec print_list lst str =
  match lst with 
  | [] -> str
  | (x, y) :: t -> 
    if str = "" then 
      let str_pair = "(" ^ Char.escaped x ^ ", " ^ string_of_int y ^ ")" in 
      print_list t str_pair
    else 
      let str_pair = ", (" ^ Char.escaped x ^ ", " ^ string_of_int y ^ ")" in 
      print_list t (str ^ str_pair)

let rec update_board start_pos end_pos state = 
  try 
    let result' = State.move start_pos end_pos state in
    let state' = State.result_parser result' in
    let board = State.current_board state' in
    let () = Gui.print_board board in 
    if Move.game_over board 
    then game_over () 
    else computer_move state'
  with _ -> print_invalid_move (); player_move state

and redo_board state = 
  try
    let result' = State.redo_move state in
    let state' = State.result_parser result' in
    let result'' = State.redo_move state' in
    let state'' = State.result_parser result'' in
    let board = State.current_board state'' in
    let () =Gui.print_board board in 
    if Move.game_over board 
    then game_over () 
    else player_move state''
  with _ -> print_invalid_command (); player_move state

and undo_board state = 
  try
    let result' = State.undo_move state in
    let state' = State.result_parser result' in
    let result'' = State.undo_move state' in
    let state'' = State.result_parser result'' in
    let board = State.current_board state'' in
    let () = Gui.print_board board in 
    if Move.game_over board 
    then game_over () 
    else player_move state''
  with _ -> print_invalid_command (); player_move state

and print_moves pos state =
  let board = State.current_board state in
  let poss_moves = Move.legal_moves pos board in 
  let () = print_string "\nThe possible moves for this position are: " in 
  if poss_moves = [] 
  then print_string "none - can't move this piece"
  else print_endline ((print_list poss_moves "")); 
  player_move state

and player_move state =
  print_enter_command ();
  try match (Command.parse (read_line ())) with
    | Quit -> exit 0
    | Instructions -> print_instructions (); player_move state
    | Move (start_pos, end_pos) -> update_board start_pos end_pos state
    | Redo -> redo_board state
    | Undo -> undo_board state
    | Moves (pos) -> print_moves pos state
    | _ -> print_invalid_command (); player_move state
  with _ ->  print_invalid_command (); player_move state

(** [computer_move board] moves one of the computer's pieces, either randomly 
    or using the minimax algorithm, depending on the difficulty of the game, 
    and calls [next_move board] to allow the game to proceed. *)
and computer_move state =
  Unix.sleep 1;
  try_to_move state;
  Unix.sleep 1;
  next_move state true

(** [next_move board players_turn] takes in a board [board] and a boolean 
    [players_turn] and, after printing the board and determining if there is a 
    checkmate, calls the correct function to facilitate the moving of either 
    the player, if [players_turn] is true, or the computer, if [players_turn] is 
    false. If there is a checkmate, this function calls [game_over ()] which 
    effectively ends the game. *)
and next_move state players_turn = 
  let board = State.current_board state in 
  let () = Gui.print_board board in 
  if Move.game_over board 
  then game_over () 
  else if players_turn 
  then player_move state
  else computer_move state

let play_game () = 
  let state = State.init_state () in 
  let board = State.current_board state in 
  let () = Gui.print_board board in 
  player_move state

(** [main is_start] prompts for the game to play, then starts it. Prints
    start page if [is_start] is true. *)
let rec main is_start = 
  if is_start 
  then Gui.print_start_page () 
  else print_enter_command ();
  try match Command.parse (read_line ()) with
    | Instructions -> Gui.print_instructions (); main false
    | Start level -> depth := level; play_game ()
    | Quit -> exit 0
    | _ -> print_invalid_command (); main false
  with _ -> print_invalid_command (); main false

(* Execute the game engine. *)
let () = main true
