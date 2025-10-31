let rec atLeastOneFactor num factor =
    if (factor == num || factor == 1) then false else
        match num mod factor with
        | 0 -> true
        | _ -> atLeastOneFactor num (factor+1)

let isPrime r = match r with
    | 0 -> false
    | 1 -> true
    | _ -> atLeastOneFactor r 2 |> not

let primesInRange st en = 
    let nums = List.init (en-st) (fun x -> x + 1) in
        List.filter (fun x -> isPrime x) nums

let rec prime_io () =
    print_string {|
        1) Is Prime
        2) Prime Numbers in Range
        0) PREVIOUS

    : |};
    let choice = read_int () in 
        match choice with 
        | 1 -> 
            let num = Io.int_input "Number: " in 
            let prime = isPrime num in
                Printf.printf "%d %s a prime number!" num (if prime then "is" else "is not")
        | 2 -> 
            let st = Io.int_input "Start: " in
            let ed = Io.int_input "End: " in 
            let nums = primesInRange st ed in
                Printf.printf "Prime Numbers: [ ";
                List.iter (fun x -> Printf.printf " %d;" x) nums;
                Printf.printf " ]"
        | 0 -> ()
        | _ -> prime_io ()


