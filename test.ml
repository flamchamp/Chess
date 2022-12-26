open OUnit2
open Board
open Command
open Move
open Piece
open State

(** Test Plan:
    The goal of this testing suit was to individually test the major features
    of the game, apart from the AI that was built using the Minimax algorithm. 
    The primary features that we sought to automatically test were features 
    implemented in the Piece, Board, Command, Move, and State modules.
    The minimax algorithm was not tested here as it is non-deterministic and 
    was thus easier to test through activiely playing the chess game. 

    The testing that was done was primarily black box, with some of the edge 
    cases tested for being a product of glass box testing i.e. knowledge of the 
    specific implementation of the functions. 

    This testing apporach demonstrates the correctness of the system as it 
    tests the implementation of all chess rules and features. The success of 
    the AI in choosing moves is difficult to measure and is best tested by
    playing the game at differering difficulties and analytically evaluating the 
    choices that the AI makes. *)

(** [cmp_set_like_lists lst1 lst2] compares two lists to see whether
    they are equivalent set-like lists.  That means checking two things.
    First, they must both be {i set-like}, meaning that they do not
    contain any duplicates.  Second, they must contain the same elements,
    though not necessarily in the same order. *)
let cmp_set_like_lists lst1 lst2 =
  let uniq1 = List.sort_uniq compare lst1 in
  let uniq2 = List.sort_uniq compare lst2 in
  List.length lst1 = List.length uniq1
  &&
  List.length lst2 = List.length uniq2
  &&
  uniq1 = uniq2

(** [pp_string s] pretty-prints string [s]. *)
let pp_string s = "\"" ^ s ^ "\""

(** [pp_pos pos] pretty-prints postion tuples [pos]. *)
let pp_pos pos = 
  let (c, i) = pos in 
  let c = Char.escaped c in
  let i = string_of_int i in 
  "('" ^ c ^ "', " ^ i ^ ")"

(** [pp_list pp_elt lst] pretty-prints list [lst], using [pp_elt]
    to pretty-print each element of [lst]. *)
let pp_list pp_elt lst =
  let pp_elts lst =
    let rec loop n acc = function
      | [] -> acc
      | [h] -> acc ^ pp_elt h
      | h1 :: (h2 :: t as t') ->
        if n = 100 then acc ^ "..."  (* stop printing long list *)
        else loop (n + 1) (acc ^ (pp_elt h1) ^ "; ") t'
    in loop 0 "" lst
  in "[" ^ pp_elts lst ^ "]"

(********************************************************************
   End helper functions.
 ********************************************************************)

let get_grade_test name piece expected_output = 
  name >:: (fun _ -> assert_equal expected_output (Piece.get_grade piece)
               ~printer:(fun exp_piece -> Piece.to_string_grade exp_piece))

let get_color_test name piece expected_output = 
  name >:: (fun _ -> assert_equal expected_output (Piece.get_color piece)
               ~printer:(fun exp_piece -> Piece.to_string_color exp_piece))

let get_point_test name piece expected_output = 
  name >:: (fun _ -> assert_equal expected_output (Piece.get_point piece)
               ~printer:Int.to_string)

let white_rook_I = Piece.init_piece Piece.Rook Piece.White
let white_rook_II = Piece.set_num_moves white_rook_I 100 

let black_pawn_I = Piece.init_piece Piece.Pawn Piece.Black
let black_pawn_II = Piece.set_num_moves black_pawn_I 1


let piece_tests = 
  [
    get_grade_test "grade of a white rook 1: rook" white_rook_I Piece.Rook;
    get_color_test "color of a white rook 1: white" white_rook_I Piece.White;
    get_point_test "points of a white rook 1: 50" white_rook_I 50;
    get_grade_test "grade of a black pawn 1: pawn" black_pawn_I Piece.Pawn;
    get_color_test "color of a black pawn 1: black" black_pawn_I Piece.Black;
    get_point_test "points of a black pawn 1: -10" black_pawn_I ~-10;
  ]

let get_piece_test name position board expected_output = 
  name >:: (fun _ -> assert_equal expected_output (get_piece position board)
               ~printer:(fun exp_piece ->
                   if exp_piece = None then "none" else Piece.to_string 
                       (Piece.option_to_piece exp_piece)))

let remove_piece_test name position board expected_output = 
  let new_board = Board.copy_board board in
  Board.remove_piece position new_board;
  get_piece_test name position new_board expected_output

let add_piece_test name position piece board expected_output = 
  let new_board = Board.copy_board board in
  Board.add_piece position piece new_board;
  get_piece_test name position new_board expected_output

let move_piece_test name initial_pos final_pos board expected_output = 
  let new_board = Board.copy_board board in
  Board.move_piece initial_pos final_pos new_board;
  get_piece_test name final_pos new_board expected_output

let board = Board.init_board ()

let board_tests = 
  [
    get_piece_test "piece at ('a',1): white rook" ('a',1) board
      (Some white_rook_I);
    get_piece_test "piece at ('a',7): black pawn" ('a',7) board
      (Some black_pawn_I);
    get_piece_test "piece at ('f',7): black pawn" ('f',7) board
      (Some black_pawn_I);
    get_piece_test "piece at ('d',5): none" ('d',5) board None;
    remove_piece_test "remove piece at ('a',1), piece at ('a',1): none" ('a',1) 
      board None;
    add_piece_test "add a white rook to ('c', 5), piece at ('c',5): white rook" 
      ('c',5) (Some white_rook_I) board (Some white_rook_I);
  ]

let create_tuple_test name object_phrase expected_output = 
  name >:: (fun _ -> assert_equal expected_output 
               (create_tuple (String.split_on_char ' ' object_phrase)))

let parse_test name command expected_output = 
  name >:: (fun _ -> assert_equal expected_output (parse command))

let parse_exception_test name command exc = 
  name >:: (fun _ -> assert_raises exc (fun _ -> parse command))

let command_tests = 
  [
    create_tuple_test "test create tuple" "a1 a2" (('a',1),('a',2));
    parse_test "test move" "move a1 a2" 
      (Command.Move (create_tuple (String.split_on_char ' ' "a1 a2")));
    parse_test "test instruction" "instructions" Command.Instructions;
    parse_test "test quit" "quit" Command.Quit;
    parse_test "test undo" "undo" Command.Undo;
    parse_test "test redo" "redo" Command.Redo;
  ]

let board_I = Board.init_board ()

let board_white_pawn_II = Board.copy_board board_I
let () = Board.move_piece ('a', 2) ('a', 3) board_white_pawn_II

let board_white_pawn_III = Board.copy_board board_I
let () = Board.move_piece ('a', 2) ('a', 4) board_white_pawn_III
let () = Board.move_piece ('b', 7) ('b', 5) board_white_pawn_III

let board_black_pawn_I = Board.copy_board board_I
let () = Board.move_piece ('b', 7) ('b', 6) board_black_pawn_I

let board_black_pawn_II = Board.copy_board board_I
let () = Board.move_piece ('a', 2) ('a', 4) board_black_pawn_II
let () = Board.move_piece ('b', 7) ('b', 5) board_black_pawn_II


let board_white_knight_I = Board.copy_board board_I

let board_white_knight_II = Board.copy_board board_white_knight_I
let () = Board.move_piece ('b', 1) ('c', 3) board_white_knight_II
let () = Board.move_piece ('c', 3) ('d', 5) board_white_knight_II
let () = Board.move_piece ('c', 7) ('c', 6) board_white_knight_II
let () = Board.move_piece ('e', 7) ('e', 6) board_white_knight_II

let board_black_knight_I = Board.copy_board board_I

let board_black_knight_II = Board.copy_board board_black_knight_I
let () = Board.move_piece ('g', 8) ('f', 6) board_black_knight_II
let () = Board.move_piece ('f', 6) ('e', 4) board_black_knight_II
let () = Board.move_piece ('d', 2) ('d', 3) board_black_knight_II
let () = Board.move_piece ('f', 2) ('f', 3) board_black_knight_II

let board_white_rook_I = Board.copy_board board_I

let board_white_rook_II = Board.copy_board board_white_rook_I
let () = Board.move_piece ('a', 2) ('a', 4) board_white_rook_II

let board_white_rook_III = Board.copy_board board_white_rook_I
let () = Board.move_piece ('b', 1) ('c', 3) board_white_rook_III
let () = Board.move_piece ('d', 2) ('d', 3) board_white_rook_III
let () = Board.move_piece ('c', 1) ('d', 2) board_white_rook_III

let board_white_rook_IV = Board.copy_board board_white_rook_I
let () = Board.move_piece ('a', 2) ('a', 4) board_white_rook_IV
let () = Board.move_piece ('b', 1) ('c', 3) board_white_rook_IV
let () = Board.move_piece ('d', 2) ('d', 3) board_white_rook_IV
let () = Board.move_piece ('c', 1) ('d', 2) board_white_rook_IV

let board_white_rook_V = Board.copy_board board_white_rook_I
let () = Board.move_piece ('g', 1) ('f', 3) board_white_rook_V
let () = Board.move_piece ('g', 2) ('g', 3) board_white_rook_V
let () = Board.move_piece ('f', 1) ('g', 2) board_white_rook_V

let board_white_rook_VI = Board.copy_board board_white_rook_I
let () = Board.move_piece ('h', 2) ('h', 4) board_white_rook_VI
let () = Board.move_piece ('g', 1) ('f', 3) board_white_rook_VI
let () = Board.move_piece ('g', 2) ('g', 3) board_white_rook_VI
let () = Board.move_piece ('f', 1) ('g', 2) board_white_rook_VI

let board_black_queen_I = Board.copy_board board_I
let () = Board.move_piece ('d', 7) ('d', 5) board_black_queen_I

let board_black_queen_II = Board.copy_board board_I
let () = Board.move_piece ('d', 7) ('d', 5) board_black_queen_II
let () = Board.move_piece ('d', 8) ('d', 6) board_black_queen_II

let board_black_queen_III = Board.copy_board board_I
let () = Board.move_piece ('d', 7) ('d', 5) board_black_queen_III
let () = Board.move_piece ('d', 8) ('d', 6) board_black_queen_III
let () = Board.move_piece ('d', 6) ('e', 5) board_black_queen_III

let board_white_bishop_I = Board.copy_board board_I
let () = Board.move_piece ('d', 2) ('d', 3) board_white_bishop_I

let board_white_bishop_II = Board.copy_board board_I
let () = Board.move_piece ('d', 2) ('d', 3) board_white_bishop_II
let () = Board.move_piece ('b', 2) ('b', 3) board_white_bishop_II

let board_white_king_I = Board.copy_board board_I
let () = Board.move_piece ('e', 2) ('e', 3) board_white_king_I

let board_white_king_II = Board.copy_board board_I
let () = Board.move_piece ('d', 2) ('d', 3) board_white_king_II

let board_white_king_III = Board.copy_board board_I
let () = Board.move_piece ('e', 2) ('e', 3) board_white_king_III
let () = Board.move_piece ('d', 2) ('d', 3) board_white_king_III

let board_white_king_IV = Board.copy_board board_I
let () = Board.remove_piece ('b', 1) board_white_king_IV
let () = Board.remove_piece ('c', 1) board_white_king_IV
let () = Board.remove_piece ('d', 1) board_white_king_IV
let () = Board.remove_piece ('f', 1) board_white_king_IV
let () = Board.remove_piece ('g', 1) board_white_king_IV

let board_white_king_V = Board.copy_board board_I
let () = Board.remove_piece ('b', 1) board_white_king_V
let () = Board.remove_piece ('c', 1) board_white_king_V
let () = Board.remove_piece ('d', 1) board_white_king_V
let () = Board.remove_piece ('f', 1) board_white_king_V
let () = Board.remove_piece ('g', 1) board_white_king_V
let () = Board.remove_piece ('h', 1) board_white_king_V

let board_white_king_VI = Board.copy_board board_I
let () = Board.remove_piece ('a', 1) board_white_king_VI
let () = Board.remove_piece ('b', 1) board_white_king_VI
let () = Board.remove_piece ('c', 1) board_white_king_VI
let () = Board.remove_piece ('d', 1) board_white_king_VI
let () = Board.remove_piece ('f', 1) board_white_king_VI
let () = Board.remove_piece ('g', 1) board_white_king_VI
let () = Board.remove_piece ('h', 1) board_white_king_VI

let board_white_king_VII = Board.copy_board board_white_king_IV
let () =  Board.move_piece ('e', 1) ('f', 1) board_white_king_VII

let board_black_king_I = Board.copy_board board_I
let () = Board.remove_piece ('b', 8) board_black_king_I
let () = Board.remove_piece ('c', 8) board_black_king_I
let () = Board.remove_piece ('d', 8) board_black_king_I
let () = Board.remove_piece ('f', 8) board_black_king_I
let () = Board.remove_piece ('g', 8) board_black_king_I

let board_black_king_II = Board.copy_board board_I
let () = Board.remove_piece ('b', 8) board_black_king_II
let () = Board.remove_piece ('c', 8) board_black_king_II
let () = Board.remove_piece ('d', 8) board_black_king_II
let () = Board.remove_piece ('f', 8) board_black_king_II
let () = Board.remove_piece ('g', 8) board_black_king_II
let () = Board.remove_piece ('h', 8) board_black_king_II

let board_black_king_III = Board.copy_board board_I
let () = Board.remove_piece ('a', 8) board_black_king_III
let () = Board.remove_piece ('b', 8) board_black_king_III
let () = Board.remove_piece ('c', 8) board_black_king_III
let () = Board.remove_piece ('d', 8) board_black_king_III
let () = Board.remove_piece ('f', 8) board_black_king_III
let () = Board.remove_piece ('g', 8) board_black_king_III
let () = Board.remove_piece ('h', 8) board_black_king_III

let board_black_king_IV = Board.copy_board board_black_king_I
let () =  Board.move_piece ('e', 8) ('f', 8) board_black_king_IV

let enpassant_white_pawn_I = Board.copy_board board_I
let () = Board.move_piece ('b', 2) ('b', 4) enpassant_white_pawn_I
let () = Board.move_piece ('b', 4) ('b', 5) enpassant_white_pawn_I
let () = Board.move_piece ('a', 7) ('a', 5) enpassant_white_pawn_I

let enpassant_white_pawn_II = Board.copy_board board_I
let () = Move.move ('b', 2) ('b', 4) enpassant_white_pawn_II
let () = Move.move ('b', 4) ('b', 5) enpassant_white_pawn_II
let () = Move.move ('a', 7) ('a', 5) enpassant_white_pawn_II
let () = Move.move ('c', 7) ('c', 5) enpassant_white_pawn_II

let legal_moves_test name position board expected_output =
  name >:: (fun _ -> assert_equal ~cmp:cmp_set_like_lists 
               ~printer:(pp_list pp_pos) expected_output 
               (legal_moves position board))

let move_tests = 
  [
    legal_moves_test 
      "Legal moves for white pawn, white pawn has one legal enpassant" 
      ('b', 5) enpassant_white_pawn_I [('b', 6); ('a', 6)];
    legal_moves_test 
      "Legal moves for white pawn, white pawn has two legal enpassant" 
      ('b', 5) enpassant_white_pawn_II [('b', 6); ('c', 6); ('a', 6)];

    legal_moves_test "Legal moves for white pawn, white pawn hasn't moved" 
      ('a', 2) board_I [('a', 3); ('a', 4)];
    legal_moves_test "Legal moves for white pawn, white pawn has moved"
      ('a', 3) board_white_pawn_II [('a', 4)];
    legal_moves_test "Legal moves for white pawn, piece in diagonal" 
      ('a', 4) board_white_pawn_III [('a', 5); ('b', 5)];

    legal_moves_test "Legal moves for black pawn, black pawn hasn't moved" 
      ('b', 7) board_I [('b', 6); ('b', 5)];
    legal_moves_test "Legal moves for black pawn, black pawn has moved"
      ('b', 6) board_black_pawn_I [('b', 5)];
    legal_moves_test "Legal moves for black pawn, piece in diagonal" 
      ('b', 5) board_black_pawn_II [('b', 4); ('a', 4)];

    legal_moves_test "Legal moves for white knight, white knight hasn't moved"
      ('b', 1) board_white_knight_I [('a', 3); ('c', 3)];
    legal_moves_test 
      "Legal moves for white knight, all possible positions are open"
      ('d', 5) board_white_knight_II 
      [('b', 4); ('b', 6); ('c', 3); ('c', 7); ('e', 3); ('e', 7); ('f', 4); 
       ('f', 6)];


    legal_moves_test "Legal moves for black knight, intial board"
      ('b', 8) board_black_knight_I [('a', 6); ('c', 6)];
    legal_moves_test 
      "Legal moves for black knight, all possible positions are open"
      ('e', 4) board_black_knight_II 
      [('c', 3); ('c', 5); ('d', 2); ('d', 6); ('f', 2); ('f', 6); ('g', 3); 
       ('g', 5)]; 

    legal_moves_test "Legal moves for white rook, initial board" 
      ('a', 1) board_white_rook_I [];
    legal_moves_test "Legal moves for white rook, open forward spaces"
      ('a', 1) board_white_rook_II [('a', 2) ; ('a', 3)];
    legal_moves_test "Legal moves for white rook, open right spaces"
      ('a', 1) board_white_rook_III [('b', 1) ; ('c', 1)];
    legal_moves_test "Legal moves for white rook, open forward and right spaces"
      ('a', 1) board_white_rook_IV [('a', 2) ; ('a', 3); ('b', 1) ; ('c', 1)];
    legal_moves_test "Legal moves for white rook, open left spaces"
      ('h', 1) board_white_rook_V [('g', 1) ; ('f', 1)];
    legal_moves_test "Legal moves for white rook, open forward and left spaces"
      ('h', 1) board_white_rook_VI [('h', 2) ; ('h', 3) ; ('g', 1) ; ('f', 1)];

    legal_moves_test "Legal moves for white bishop, initial board"
      ('c', 1) board_I [];
    legal_moves_test "Legal moves for white bishop, one open diagonal"
      ('c', 1) board_white_bishop_I [('d', 2) ; ('e', 3) ; ('f', 4) ; ('g', 5) ; 
                                     ('h', 6)];
    legal_moves_test "Legal moves for white bishop, multiple open diagonals"
      ('c', 1) board_white_bishop_II [('b', 2) ; ('a', 3) ; ('d', 2) ; ('e', 3); 
                                      ('f', 4) ; ('g', 5) ; ('h', 6)];

    legal_moves_test "Legal moves for black queen, initial board"
      ('d', 8) board_I [];
    legal_moves_test "Legal moves for black queen, open forward"
      ('d', 8) board_black_queen_I [('d', 7); ('d', 6)];
    legal_moves_test 
      "Legal moves for black queen, open row and forward diagonals"
      ('d', 6) board_black_queen_II [('c', 5); ('b', 4); ('a', 3); ('e', 5); 
                                     ('f', 4); ('g', 3); ('h', 2); ('e', 6); 
                                     ('f', 6); ('g', 6); ('h', 6); ('c', 6); 
                                     ('b', 6); ('a', 6); ('d', 7); ('d', 8)];
    legal_moves_test "Legal moves for black queen, all directions"
      ('e', 5) board_black_queen_III [('f', 6); ('d', 4); ('c', 3); ('b', 2); 
                                      ('d', 6); ('f', 4); ('g', 3); ('h', 2); 
                                      ('f', 5); ('g', 5); ('h', 5); ('e', 6); 
                                      ('e', 4); ('e', 3); ('e', 2)];

    legal_moves_test "Legal moves for white king, initial board"
      ('e', 1) board_I [];
    legal_moves_test "Legal moves for white king, open forward"
      ('e', 1) board_white_king_I [('e', 2)];
    legal_moves_test "Legal moves for white king, open diagonal"
      ('e', 1) board_white_king_II [('d', 2)];
    legal_moves_test "Legal moves for white king, open diagonal and forward"
      ('e', 1) board_white_king_III [('d', 2) ; ('e', 2)];

    legal_moves_test "White king castling to the queen or king side" ('e', 1)
      board_white_king_IV [('g', 1); ('c', 1); ('d', 1); ('f', 1)];
    legal_moves_test "Black king castling to the queen or king side" ('e', 8)
      board_black_king_I [('g', 8); ('c', 8); ('d', 8); ('f', 8)];

    legal_moves_test "White king castling to the queen" ('e', 1)
      board_white_king_V [('c', 1); ('d', 1); ('f', 1)];
    legal_moves_test "Black king castling to the queen" ('e', 8)
      board_black_king_II [('c', 8); ('d', 8); ('f', 8)];

    legal_moves_test "White king can't castle, no rooks" ('e', 1)
      board_white_king_VI [('d', 1); ('f', 1)];
    legal_moves_test "Black king can't, no rooks" ('e', 8)
      board_black_king_III [('d', 8); ('f', 8)];

    legal_moves_test "White king can't castle, has moved" ('f', 1)
      board_white_king_VII [('e', 1); ('g', 1)];
    legal_moves_test "Black king can't, has moved" ('f', 8)
      board_black_king_IV [('e', 8); ('g', 8)];
  ] 

let init_state = State.init_state ()
let init_board = Board.init_board ()

let first_state = State.init_state ()
let first_board = Board.init_board ()

let second_result = State.move ('a', 2) ('a', 4) first_state
let second_state = State.result_parser second_result
let second_board = Board.copy_board first_board
let () = Board.move_piece ('a', 2) ('a', 4) second_board

let third_result = State.undo_move second_state
let third_state = State.result_parser third_result
let third_board = first_board

let fourth_result = State.redo_move third_state
let fourth_state = State.result_parser fourth_result
let fourth_board = second_board

let current_board_test name state expected_output =
  name >:: (fun _ -> assert_equal ~cmp:cmp_set_like_lists 
               (expected_output |> bindings) 
               (state |> current_board |> bindings ))

let move_exception_test name initial_pos final_pos st exc = 
  name >:: (fun _ -> assert_equal exc (State.move initial_pos final_pos st))

let redo_exception_test name st exc = 
  name >:: (fun _ -> assert_equal exc (redo_move st))

let undo_exception_test name st exc = 
  name >:: (fun _ -> assert_equal exc (undo_move st))

let state_tests = 
  [
    current_board_test "the initial state is the initial board"
      init_state init_board;

    move_exception_test "illegal pawn move of 3 squares" ('a', 2) ('a', 5)
      init_state (Illegal "Illegal move");

    move_exception_test "no piece to move" ('g', 6) ('c', 5)
      init_state (Illegal "No piece to move");

    redo_exception_test "can't redo a move when the game just started"
      init_state (Illegal "No moves to redo");

    undo_exception_test "can't undo a move when the game just started"
      init_state (Illegal "No moves to undo");

    current_board_test "current board after moving pawn ('a', 2) to ('a', 4)"
      second_state second_board;

    current_board_test "the undo board after moving the pawn back to ('a', 2) 
    from ('a', 4) is the first board" third_state third_board;

    current_board_test "the redo board after moving pawn back to ('a', 4) from 
    ('a', 2) is the second board" fourth_state fourth_board;
  ]

let suite =
  "test suite for chess milestone"  >::: List.flatten [
    piece_tests;
    board_tests;
    command_tests;
    move_tests;
    state_tests;
  ]

let _ = run_test_tt_main suite
