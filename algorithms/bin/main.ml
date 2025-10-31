let rec main () = 
    print_string {|

        Devisha's OCaml Learning Project!

        1) Password Cracking Test (Simulate brute forcing to see how strong a random password is)
        2) Prime Numbers
        0) Exit

    : |};
    let choice = read_int () in
        match choice with
        |   1 -> Password.password_crack_test (); main ()
        |   2 -> Prime.prime_io (); main ()
        |   0 -> ()
        |   _ -> main ()


let () = main ()

