let n = int_of_string (input_line stdin) in
let rec process x =
  print_int(x); print_char(' ');
  match x with
  | 1 -> ()
  | x when (x mod 2) = 0 -> process (x / 2)
  | _ -> process ((x * 3) + 1)
in process n;;