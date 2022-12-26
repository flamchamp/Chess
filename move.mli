open Board

exception IllegalMove
exception EmptyPosition
exception ImpossibleBranchReached

(** [legal_moves position board] produces an array of legal moves for the piece 
    in the position [position] on the board [board]. If the specified position 
    is empty, returns an empty list. *)
val legal_moves: Board.position -> Board.t -> Board.position list 

(** [is_on_board position board] returns true if the position [position] is on 
    the board [board] 
    (i.e. for a position (x,y), returns true if x is in ['A':'H'] and y is in 
    [1:8]) *)
val is_on_board: Board.position -> Board.t -> bool 

(** [contains_piece position board] returns true if there is a piece in the 
    position [position] on the board [board]. *)
val contains_piece: Board.position -> Board.t -> bool

(** [is_legal start_position end_position board] determines if a specified move, 
    from [start_position] to [end_position], is legal based on the moves of 
    chess on the board [board]. *)
val is_legal: Board.position -> Board.position -> Board.t -> bool

(**  [all_legal_moves color board] produces a list of all of the legal moves
     for the player whose color is [color] to move on the board [board]. The 
     list is a list of tuples in the format (start_position, end_position) 
     representing unique moves that the player can make from a given start
     position on the board to to a given end positon on the board. *)
val all_legal_moves: Piece.color -> Board.t -> (Board.position * Board.position)
    list

(** [move start_position end_position board] moves the piece in [start_position] 
    to [end_position] if the move is legal. If it is illegal, an IllegalMove 
    exception is raised. *)
val move: Board.position -> Board.position -> Board.t -> unit 

(** [is_in_check color board] returns true if the player whose color is [color]
    if in check. *)
val is_in_check: Piece.color -> Board.t -> bool

(** [game_over board] outputs true if the game is over, i.e. a king is in 
    checkmate and false otherwise.  If the move from [start_pos]
    to [end_pos] is illegal, an IllegalMove exception is raised.*)
val game_over: Board.t -> bool
