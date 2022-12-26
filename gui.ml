open Piece
open Board

let read_file filename =
  let ch = open_in filename in
  let s = really_input_string ch (in_channel_length ch) in
  close_in ch;
  s

let print_start_page () = 
  ANSITerminal.erase Above;
  ANSITerminal.(print_string [red] (read_file "start_page.txt"));
  ANSITerminal.(print_string [white] 
                  "\n\nReady to play (type start) or need to read instructions \
                   (type instructions)?\n\nEnter a command\n> ")

let print_instructions () = 
  ANSITerminal.erase Above;
  ANSITerminal.(print_string [white] (read_file "instructions.txt"));
  print_string "\n"

let print_board board = 
  ANSITerminal.erase Above;
  ANSITerminal.(print_string [white] (read_file "board_header.txt"));
  Board.print_board board;
  ANSITerminal.(print_string [white] (read_file "board_footer.txt"));
  print_string "\n"
