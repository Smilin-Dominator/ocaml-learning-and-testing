
type password_digits = int*int*int

let all_letters = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"
let all_digits = "1234567890"
let all_punc = "!@#$%^&*()-_=+{[\\}]|:;\"'?/>.<,~`'}"

let password_digit_allocation length ?(letters=0) ?(digits=0) ?(punc=0) () =
    let n_letters = if letters <> 0 then letters else Random.int_in_range ~min:1 ~max:(length-2) in
    let n_digits = if digits <> 0 then digits else Random.int_in_range ~min:1 ~max:(length-n_letters-1) in
    let n_punc = if digits <> 0 then punc else length - n_digits - n_letters in
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

let rec compare_loop (pw: string) (digits: password_digits) attempt =
    Printf.printf "Attempt %d\n" attempt;
    let new_pw = generate_password_letters digits in
        if new_pw = pw then (Sys.time(), attempt) else compare_loop pw digits (attempt+1)

let () = 
    Random.self_init ();
    print_endline "____ PW ____";
    (* Generate the first PW *)
    let allocation = password_digit_allocation 10 () in
    let pw = generate_password_letters allocation in
        print_endline pw;
    (* Now Compare *)
    let t = Sys.time() in
    let (t1, attempt) = compare_loop pw allocation 1 in
        Printf.printf "Time Taken: %.10f seconds and %d attempts" (t1 -. t) attempt






