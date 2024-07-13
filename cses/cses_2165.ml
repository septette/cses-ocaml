open Core

let read_int () = 
  match In_channel.input_line In_channel.stdin with
  | Some line -> Int.of_string line
  | None -> raise Exit

let solve n =
  let rec hanoi n src dest free =
    if n = 1 then
      printf "%d %d\n" src dest
    else (
        hanoi (n-1) src free dest;
        printf "%d %d\n" src dest;
        hanoi (n-1) free dest src
    )
  in hanoi n 1 3 2

let () =
  let n = read_int () in
  printf "%d\n" ((Int.pow 2 n) - 1);
  solve n