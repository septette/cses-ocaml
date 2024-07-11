open Core

let read_int () = 
  match In_channel.input_line In_channel.stdin with
  | Some line -> Int.of_string line
  | None -> raise Exit

let parse_input () = 
  read_int ()

let solve n =
  let rec helper n remaining acc_set =
    (
      if remaining = 0 then
        Some acc_set
      else if not (Int.equal (remaining - n) n) then
        let to_add = min remaining n in
        helper (n - 1) (remaining - to_add) (Set.add acc_set to_add)
      else if n > 0 then
        helper (n - 1) remaining acc_set
      else
        None
    )
  in
  let total = n * (n + 1) / 2 in
  if total % 2 = 1 then
    None
  else
    helper n (total / 2) Int.Set.empty

let verify set_1 n = 
  match set_1 with
  | None -> print_endline "NO"
  | Some set ->
    print_endline "YES";
    print_endline (string_of_int (Set.length set));
    Set.iter ~f:(printf "%d ") set;
    printf "\n";
    print_endline (string_of_int (n - (Set.length set)));
    for i = n downto 1 do
      if Set.mem set i then () else
      printf "%d " i
    done

let () =
  let n = parse_input () in
  verify (solve n) n