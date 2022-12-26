type color = 
  | Black 
  | White

type grade = 
  | Pawn 
  | Bishop 
  | Knight 
  | Rook 
  | Queen 
  | King

exception NonePiece

type t = {
  grade: grade;
  color: color;
  points: int;
  moves: int
}

let init_piece grade color =
  let points = match grade with
    | Pawn -> 10
    | Bishop -> 30
    | Knight -> 30
    | Rook -> 50
    | Queen -> 90
    | King -> 900
  in let points = if color = Black 
       then -1 * points 
       else points
  in { grade = grade; 
       color = color; 
       points = points; 
       moves = 0 }

let get_grade piece = 
  piece.grade

let get_color piece = 
  piece.color

let get_point piece = 
  piece.points

let get_num_moves piece = 
  piece.moves

let set_num_moves piece num_moves = 
  { piece with moves = num_moves }

let to_string_grade grade =
  match grade with
  | Pawn -> "pawn"
  | Bishop -> "bishop"
  | Knight -> "knight"
  | Rook -> "rook"
  | Queen -> "queen"
  | King -> "king"

let to_string_color color =
  match color with
  | Black -> "black"
  | White -> "white"

let to_string piece = 
  match get_grade piece with
  | Pawn -> if get_color piece = Black 
    then "♙" 
    else "♟"
  | Rook -> if get_color piece = Black 
    then "♖" 
    else "♜"
  | Knight -> if get_color piece = Black 
    then "♘" 
    else "♞"
  | Bishop -> if get_color piece = Black 
    then "♗" 
    else "♝"
  | Queen -> if get_color piece = Black 
    then "♕" 
    else "♛"
  | King -> if get_color piece = Black 
    then "♔" 
    else "♚"

let option_to_piece piece_op = 
  match piece_op with
  | Some piece -> piece
  | None -> raise NonePiece

let is_black_op piece_op = 
  try
    let piece = option_to_piece piece_op in 
    get_color piece = Black 
  with NonePiece -> false

let is_white_op piece_op = 
  try
    let piece = option_to_piece piece_op in 
    get_color piece = White 
  with NonePiece -> false

let pawn_multiplier = 
  [| [| 0.0; 0.0; 0.0; 0.0; 0.0; 0.0; 0.0; 0.0|];
     [| 5.0; 5.0; 5.0; 5.0; 5.0; 5.0; 5.0; 5.0|];
     [| 1.0; 1.0; 2.0; 3.0; 3.0; 2.0; 1.0; 1.0|];
     [| 0.5; 0.5; 1.0; 2.5; 2.5; 1.0; 0.5; 0.5|];
     [| 0.0; 0.0; 0.0; 2.0; 2.0; 0.0; 0.0; 0.0|];
     [| 0.5;-0.5;-1.0; 0.0; 0.0;-1.0;-0.5; 0.5|];
     [| 0.5; 1.0; 1.0;-2.0;-2.0; 1.0; 1.0; 0.5|];
     [| 0.0; 0.0; 0.0; 0.0; 0.0; 0.0; 0.0; 0.0|] |]

let bishop_multiplier = 
  [| [|-2.0;-1.0;-1.0;-1.0;-1.0;-1.0;-1.0;-2.0|];
     [|-1.0; 0.0; 0.0; 0.0; 0.0; 0.0; 0.0;-1.0|];
     [|-1.0; 0.0; 0.5; 1.0; 1.0; 0.5; 0.0;-1.0|];
     [|-1.0; 0.5; 0.5; 1.0; 1.0; 0.5; 0.5;-1.0|];
     [|-1.0; 0.0; 1.0; 1.0; 1.0; 1.0; 9.9;-1.0|];
     [|-1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0;-1.0|];
     [|-1.0; 0.5; 0.0; 0.0; 0.0; 0.0; 0.5;-1.0|];
     [|-2.0;-1.0;-1.0;-1.0;-1.0;-1.0;-1.0;-2.0|] |]

let knight_multiplier = 
  [| [|-5.0;-4.0;-3.0;-3.0;-3.0;-3.0;-4.0;-5.0|];
     [|-4.0;-2.0; 0.0; 0.0; 0.0; 0.0;-2.0;-4.0|];
     [|-3.0; 0.0; 1.0; 1.5; 1.5; 1.0; 0.0;-3.0|];
     [|-3.0; 0.5; 1.5; 2.0; 2.0; 1.5; 0.5;-3.0|];
     [|-0.5; 0.0; 0.0; 0.0; 0.0; 0.0; 0.0;-0.5|];
     [|-0.5; 0.0; 0.0; 0.0; 0.0; 0.0; 0.0;-0.5|];
     [|-4.0;-2.0; 0.0; 0.5; 0.5; 0.0;-2.0;-4.0|];
     [|-5.0;-4.0;-3.0;-3.0;-3.0;-3.0;-4.0;-5.0|] |]

let rook_multiplier = 
  [| [| 0.0; 0.0; 0.0; 0.0; 0.0; 0.0; 0.0; 0.0|];
     [| 0.5; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 0.5|];
     [|-0.5; 0.0; 0.0; 0.0; 0.0; 0.0; 0.0;-0.5|];
     [|-0.5; 0.0; 0.0; 0.0; 0.0; 0.0; 0.0;-0.5|];
     [|-0.5; 0.0; 0.0; 0.0; 0.0; 0.0; 0.0;-0.5|];
     [|-0.5; 0.0; 0.0; 0.0; 0.0; 0.0; 0.0;-0.5|];
     [|-0.5; 0.0; 0.0; 0.0; 0.0; 0.0; 0.0;-0.5|];
     [| 0.0; 0.0; 0.0; 0.5; 0.5; 0.0; 0.0; 0.0|] |]

let queen_multiplier = 
  [| [|-2.0;-1.0;-1.0;-0.5;-0.5;-1.0;-1.0;-2.0|];
     [|-1.0; 0.0; 0.0; 0.0; 0.0; 0.0; 0.0;-1.0|];
     [|-1.0; 0.0; 0.5; 0.5; 0.5; 0.5; 0.0;-1.0|];
     [|-0.5; 0.0; 0.5; 0.5; 0.5; 0.5; 0.0;-0.5|];
     [| 0.0; 0.0; 0.5; 0.5; 0.5; 0.5; 0.0;-0.5|];
     [|-1.0; 0.5; 0.5; 0.5; 0.5; 0.5; 0.0;-1.0|];
     [|-1.0; 0.0; 0.5; 0.0; 0.0; 0.0; 0.0;-1.0|];
     [|-2.0;-1.0;-1.0;-0.5;-0.5;-1.0;-1.0;-2.0|] |]

let king_multiplier = 
  [| [|-3.0;-4.0;-4.0;-5.0;-5.0;-4.0;-4.0;-3.0|];
     [|-3.0;-4.0;-4.0;-5.0;-5.0;-4.0;-4.0;-3.0|];
     [|-3.0;-4.0;-4.0;-5.0;-5.0;-4.0;-4.0;-3.0|];
     [|-3.0;-4.0;-4.0;-5.0;-5.0;-4.0;-4.0;-3.0|];
     [|-2.0;-3.0;-3.0;-4.0;-4.0;-3.0;-3.0;-2.0|];
     [|-1.0;-2.0;-2.0;-2.0;-2.0;-2.0;-2.0;-1.0|];
     [| 2.0; 2.0; 0.0; 0.0; 0.0; 0.0; 2.0; 2.0|];
     [| 2.0; 3.0; 1.0; 0.0; 0.0; 1.0; 3.0; 2.0|] |]

let multiplier position piece =
  let (col, row) = position in
  let col = int_of_char col - 97 in
  let row = if get_color piece = White 
    then 8 - row 
    else row - 1 in
  match get_grade piece with
  | Pawn -> pawn_multiplier.(row).(col)
  | Bishop -> bishop_multiplier.(row).(col)
  | Knight -> knight_multiplier.(row).(col)
  | Rook -> rook_multiplier.(row).(col)
  | Queen -> queen_multiplier.(row).(col)
  | King -> king_multiplier.(row).(col)