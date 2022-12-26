open State

(** [move_minimax board depth maximizing] moves a white piece if
    [maximizing] is true and a white piece if [maximizing] is false using the 
    minimax algorithm. The algorithm runs for a tree with depth [depth].
    NoMovesDetected if the player has no legal moves, i.e. is in checkmate. 
    The algorithm uses alpha-beta pruning to reduce run time, but still takes a 
    considerable amount of time to run for higher depths. *)
val move_minimax: State.t -> int -> bool -> State.t
