open Test
open Lwt

let suite = suite "ppx" [
    test "let"
      (fun () ->
         let%lwt x = return 3 in
         return (x + 1 = 4)
      ) ;

    test "nested let"
      (fun () ->
         let%lwt x = return 3 in
         let%lwt y = return 4 in
         return (x + y = 7)
      ) ;

    test "and let"
      (fun () ->
         let%lwt x = return 3
             and y = return 4 in
         return (x + y = 7)
      ) ;

    test "match"
      (fun () ->
         let x = Lwt.return (Some 3) in
	 match%lwt x with
	   | Some x -> return (x + 1 = 4)
	   | None -> return false
      ) ;

    test "for" (* Test for proper sequencing *)
      (fun () ->
         let r = ref [] in
         let f x =
	   let%lwt () = Lwt_unix.sleep 0.2 in Lwt.return (r := x :: !r)
	 in
         let%lwt () =
	   for%lwt x = 3 to 5 do f x done
	 in return (!r = [5 ; 4 ; 3])
      ) ;

    test "while" (* Test for proper sequencing *)
      (fun () ->
         let r = ref [] in
         let f x =
	   let%lwt () = Lwt_unix.sleep 0.2 in Lwt.return (r := x :: !r)
	 in
         let%lwt () =
	   let c = ref 2 in
	   while%lwt !c < 5 do incr c ; f !c done
	 in return (!r = [5 ; 4 ; 3])
      ) ;

    test "assert"
      (fun () ->
         let%lwt () = assert%lwt true
	 in return true
      ) ;
  ]

let _ = Test.run "ppx" [ suite ]
