type object_phrase = string list

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

let create_tuple user_command =
  let first_num = int_of_char (List.nth user_command 0).[1] - 48 in 
  let second_num = int_of_char (List.nth user_command 1).[1] - 48 in
  let first = ((List.nth user_command 0).[0], first_num) in 
  let second = ((List.nth user_command 1).[0], second_num) in
  (first, second)

let create_pair user_command = 
  let first_num = int_of_char (List.nth user_command 0).[1] - 48 in
  let first = ((List.nth user_command 0).[0], first_num) in 
  first

let parse str =
  let user_command = (String.split_on_char ' ' (String.lowercase_ascii str)) in 
  match user_command with
  | [] -> Empty
  | h :: t ->
    if h = "instructions" then Instructions
    else if h = "move" then Move (create_tuple t)
    else if h = "quit" then Quit
    else if h = "" then Empty
    else if h = "undo" then Undo
    else if h = "redo" then Redo
    else if h = "moves" then Moves (create_pair t)
    else if h = "start" then 
      (match t with 
       | n ::_ -> 
         if n = "easy" 
         then Start 0
         else if n = "medium" 
         then Start 2
         else if n = "hard"
         then Start 4
         else Malformed
       | [] -> Malformed
      )
    else Malformed
