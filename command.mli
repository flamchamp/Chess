(** [object_phrase] is a string list that represents the user's input move,
    with each element representing the starting and ending position. *)
type object_phrase = string list

(** [command] represents a player command that is decomposed
    into a verb and an object phrase or gives the instructions of chess. 
    Empty and Malformed are not exceptions, but instead handled in the
    Main compilation unit.*)
type command = 
  | Move of ((char * int) * (char * int))
  | Instructions
  | Quit
  | Undo
  | Redo
  | Start of int
  | Moves of (char * int)  
  | Empty
  | Malformed

(** [create_tuple user_command] creates a tuple of the starting and ending
    board position that the user inputs.
    Requires: [user_command] is not empty. *)
val create_tuple : object_phrase -> (char * int) * (char * int)

(** [create_pair user_command] creates a pair of the
    board position that the user inputs.
    Requires: [user_command] is not empty. *)
val create_pair : object_phrase -> (char * int)

(**[parse str] parses a player's input into a command by splitting the input
   into different elements and matching the first element with "move", 
   "instructions", "quit", or a malformed/empty string. If there are words left,
   they become the object phrase.
   Requires: [str] contains either letters, numbers, or spaces. 
   Raises: [Empty] if [str] is the empty string or only contains spaces.
   Raises: [Malformed] if [str] is not a proper command. *)
val parse : string -> command


