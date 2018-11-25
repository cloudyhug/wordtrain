let print_help () =
  print_endline ""

let expect condition error_message =
  if condition then () else begin
    print_endline error_message;
    exit 1
  end

let getlines ic =
  let rec f acc =
    let line = try Some (input_line ic) with End_of_file -> None in
    match line with
    | Some l -> f (l :: acc)
    | None -> acc
  in f []

let generate_vocabulary lines =
  let rec f acc line_number = function
    | [] -> acc
    | l :: r ->
      match String.split_on_char '$' l with
      | [word; translation] ->
        f ((word, translation, true) :: acc) (line_number + 1) r
      | _ ->
        failwith
          (Printf.sprintf "Wrong format : \"%s\" at line %d" l line_number)
  in f [] 0 lines

let take n l =
  let rec f acc n l =
    if n = 0 then (List.rev acc, l) else
    match l with
    | [] -> (List.rev acc, l)
    | h :: t -> f (h :: acc) (n - 1) t
  in f [] n l

let insert index element l =
  let (before, after) = take index l in
  before @ (element :: after)

(* 0 pt not found - 1 pt hard - 3 pts medium - 5 pts easy *)
let one_run vocabulary easy_rate =
  let rec f points = function
    | [] -> points
    | (word, translation, deja_vu) :: r ->
      Printf.printf ">> %s\n" word;
      ignore (read_line());
      Printf.printf "Answer : %s\n" translation;
      let rec ask () =
        print_string "Result ? (e/m/h/n) : ";
        match read_line() with
        | "e" ->
          let rand = Random.int 100 in
          let vocabulary' =
            if rand < easy_rate then
              insert 15 (word, translation, false) r
            else r
          in
          let points' = if deja_vu then points else points + 5 in
          f points' vocabulary'
        | "m" ->
          let vocabulary' = insert 10 (word, translation, false) r in
          let points' = if deja_vu then points else points + 3 in
          f points' vocabulary'
        | "h" ->
          let vocabulary' = insert 5 (word, translation, false) r in
          let points' = if deja_vu then points else points + 1 in
          f points' vocabulary'
        | "n" ->
          let vocabulary' = insert 2 (word, translation, false) r in
          f points vocabulary'
        | _ -> ask()
      in ask()
  in
  let pts = f 0 vocabulary in
  let max_pts = List.length vocabulary * 5 in
  float pts /. float max_pts *. 100.

let rec run vocabulary easy_rate =
  let score = one_run vocabulary easy_rate in
  Printf.printf "Score : %f%%\n" score;
  let rec ask_continue () =
    print_string "Continue ? (y/n) : ";
    let answer = read_line() in
    if answer = "y" then begin
      print_string "\n\n\n";
      run vocabulary easy_rate
    end else if answer = "n" then ()
    else ask_continue()
  in ask_continue()

let () =
  expect
    (Array.length Sys.argv = 2)
    (Printf.sprintf "Usage: ./%s [-h|--help] file" Sys.argv.(0));
  if Sys.argv.(1) = "-h" || Sys.argv.(1) = "--help" then begin
    print_help();
    exit 0
  end;
  let ic = open_in Sys.argv.(1) in
  let lines = getlines ic in
  let vocabulary = generate_vocabulary lines in
  let rec ask_easy_rate () =
    print_string "Repetition rate of easy words (0-100) ? : ";
    match int_of_string_opt (read_line()) with
    | None -> ask_easy_rate()
    | Some i -> if i < 0 || i > 100 then ask_easy_rate() else i
  in
  let easy_rate = ask_easy_rate() in
  run vocabulary easy_rate