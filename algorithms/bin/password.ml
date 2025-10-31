type password_digits = int*int*int
type digit_input_type = Mystery of int | Provided of password_digits (* Either Mystery w/ length, or provided *)

let all_letters = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"
let all_digits = "1234567890"
let all_punc = "!@#$%^&*()-_=+{[\\}]|:;\"'?/>.<,~`'}"

let password_digit_allocation length ?(letters=(-1)) ?(digits=(-1)) ?(punc=(-1)) () =
    let n_letters = if letters <> -1 then letters else Random.int_in_range ~min:0 ~max:(length) in
    let n_digits = if digits <> -1 then digits else Random.int_in_range ~min:0 ~max:(length-n_letters) in
    let n_punc = if digits <> -1 then punc else length - n_digits - n_letters in
        Printf.printf "Length: %d = %d (Letters) + %d (Numbers) + %d (Punctuation)\n" length n_letters n_digits n_punc;
        (n_letters, n_digits, n_punc)


let rec generate_random max_index n_digits =
    match n_digits with
    | 0 -> []
    | _ -> Random.int_in_range ~min:0 ~max:max_index :: generate_random max_index (n_digits-1)

let map_to_str int_ar (str: string) =
    let char_ar = List.map (fun index -> str.[index]) int_ar in
    List.fold_left (fun str chr -> str ^ String.make 1 chr) "" char_ar

let generate_password_section n_digits (str: string) = 
    let int_arr = generate_random (-1 + String.length str) n_digits in
        map_to_str int_arr str

let generate_password_letters ((l, d, p): password_digits) = 
    let pw = generate_password_section l all_letters ^ generate_password_section d all_digits ^ generate_password_section p all_punc in
        Printf.printf "Password: %s\n\n" pw;
        pw

let rec compare_loop (pw: string) (digit_input: digit_input_type) attempt =
    let digits = match digit_input with
        | Mystery length -> password_digit_allocation length ()
        | Provided d -> d in
    Printf.printf "Attempt %d\n" attempt;
    let new_pw = generate_password_letters digits in
        if new_pw = pw then (Sys.time(), attempt) else compare_loop pw digit_input (attempt+1)

let read_int_input msg = print_string msg; read_int ()

let password_crack_test () = 
    Random.self_init ();
    print_endline "---- INPUT (-1 if you want to randomize) ----";
    (* Get the parameters from the user *)
    let length = read_int_input "Total Length: " in 
    let n_letters = read_int_input "Number of Letters: " in 
    let n_digits = read_int_input "Number of Digits: " in 
    let n_punc = read_int_input "Number of Special Characters: " in
    (* Generate the first PW *)
        print_endline "____ Password ____";
    let allocation = password_digit_allocation length ~letters:n_letters ~digits:n_digits ~punc:n_punc () in
    let pw = generate_password_letters allocation in
    (* Hard Mode *)
    let show_allocation = read_int_input "Show Allocation to Program? (1/0): " in
    let given_digits = if show_allocation <> 0 then Provided allocation else Mystery length in
    (* Now Compare *)
    let t = Sys.time() in
    let (t1, attempt) = compare_loop pw given_digits 1 in
        Printf.printf "Time Taken: %.8f seconds and %d attempts\n" (t1 -. t) attempt




