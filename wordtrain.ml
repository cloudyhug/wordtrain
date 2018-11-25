(** Prints help for this program. *)
let print_help () =
  print_endline "===== ===== ===== ===== Wordtrain ===== ===== ===== =====";
  print_endline "               Vocabulary training program               ";
  print_endline "===== ===== ===== ===== ========= ===== ===== ===== =====";
  print_newline();
  print_endline "Call the program with an input file having vocabulary";
  print_endline "translations in the following format :";
  print_endline "                    \"word$translation\"";
  print_endline "It will run a training session so that the user will be";
  print_endline "able to know the vocabulary contained in this file."

(** Gets all the lines from the input channel until End_of_file is raised. *)
let getlines ic =
  let rec f acc =
    let line = try Some (input_line ic) with End_of_file -> None in
    match line with
    | Some l -> f (l :: acc)
    | None -> acc
  in f []

(** Takes all the lines and generates vocabulary training items.
  * An example is a tuple (word, translation, deja_vu), where
  * deja_vu is a boolean indicating if the example has already been
  * seen before during the training session.
  *)
let generate_vocabulary lines =
  let rec f acc line_number = function
    | [] -> acc
    | l :: r ->
      match String.split_on_char '$' l with
      | [word; translation] ->
        f ((word, translation, false) :: acc) (line_number + 1) r
      | _ ->
        failwith
          (Printf.sprintf "Wrong format : \"%s\" at line %d" l line_number)
  in f [] 1 lines

(** Takes the first [n] elements from list [l]. If there is less than [n]
  * elements in [l], takes [l]. It returns a tuple with what was taken and
  * what is left in the list.
  *)
let take n l =
  if (n < 0) then failwith "take : n < 0" else
  let rec f acc n l =
    if n = 0 then (List.rev acc, l) else
    match l with
    | [] -> (List.rev acc, l)
    | h :: t -> f (h :: acc) (n - 1) t
  in f [] n l

(** Inserts [element] at place [index] in the list [l]. *)
let insert element index l =
  let (before, after) = take index l in
  before @ (element :: after)

(** Executes a training session with the given vocabulary and repetition rate
  * for the easy words. This rate is the percentage of easy words that will be
  * re-inserted into the list after being guessed. It allows the user to train
  * easy words too. For each example, prints the word, waits for the user to
  * press Enter, asks if the word was guessed or not, and puts back the word
  * into the training list or not, and at a defined place, according to
  * specific rules. Moreover, a score is kept from the beginning to the end of
  * the session, in order for the user to know whether he was good during this
  * session.
  * Not found -> 0 pt
  * Hardly guessed -> 1 pt
  * Medium -> 3 pts
  * Easily guessed -> 5 pts
  *)
let one_run vocabulary easy_rate =
  let rec f points = function
    | [] -> points
    | (word, translation, deja_vu) :: r ->
      Printf.printf "===== ===== =====>   %s\n" word;
      print_string "[enter]";
      ignore (read_line());
      Printf.printf "===== ===== Answer : %s\n" translation;
      let rec ask () =
        print_string "Result ? (e/m/h/n) : ";
        match read_line() with
        | "e" -> begin
          let rand = Random.int 100 in
          let vocabulary' =
            if rand < easy_rate then
              insert (word, translation, true) 15 r
            else r
          in
          let points' = if deja_vu then points else points + 5 in
          print_newline();
          f points' vocabulary'
        end
        | "m" -> begin
          let vocabulary' = insert (word, translation, true) 10 r in
          let points' = if deja_vu then points else points + 3 in
          print_newline();
          f points' vocabulary'
        end
        | "h" -> begin
          let vocabulary' = insert (word, translation, true) 5 r in
          let points' = if deja_vu then points else points + 1 in
          print_newline();
          f points' vocabulary'
        end
        | "n" -> begin
          let vocabulary' = insert (word, translation, true) 2 r in
          print_newline();
          f points vocabulary'
        end
        | _ -> ask()
      in ask()
  in
  let pts = f 0 vocabulary in
  let max_pts = List.length vocabulary * 5 in
  float pts /. float max_pts *. 100.

(** Runs a training session over and over until the user decides to stop. *)
let rec run vocabulary easy_rate =
  let score = one_run vocabulary easy_rate in
  print_newline();
  Printf.printf "===== ===== Score : %.2f %%\n" score;
  let rec ask_continue () =
    print_string "Continue ? (y/n) : ";
    let answer = read_line() in
    if answer = "y" then begin
      print_string "\n\n\n";
      run vocabulary easy_rate
    end else if answer = "n" then ()
    else ask_continue()
  in ask_continue()

(* Main function *)
let () =
  let () =
    match Array.length Sys.argv with
    | 2 ->
      if Sys.argv.(1) = "-h" || Sys.argv.(1) = "--help" then begin
        print_help();
        exit 0
      end else ()
    | _ -> begin
      Printf.printf "Usage : %s [-h | --help | file]\n" Sys.argv.(0);
      exit 1
    end
  in
  let ic = open_in Sys.argv.(1) in
  let lines = getlines ic in
  let vocabulary = generate_vocabulary lines in
  let rec ask_easy_rate () =
    print_string "Repetition rate for the easy words (0-99) ? : ";
    match int_of_string_opt (read_line()) with
    | None -> ask_easy_rate()
    | Some i -> if i < 0 || i > 99 then ask_easy_rate() else i
  in
  let easy_rate = ask_easy_rate() in
  print_newline();
  run vocabulary easy_rate