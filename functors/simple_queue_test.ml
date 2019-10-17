
open Core

let demo_create () =
  let _ = Simple_queue.empty in
  ()
;;

let dequeue_prt q = 
  match (Simple_queue.dequeue q) with
    | None -> 
      printf "__empty__\n";
      Simple_queue.empty
    | Some (v, q') -> 
      printf "dequeue: %d\n" v;
      q'
;;

let demo_enqueue_dequeue () =
  let q = Simple_queue.empty in
  let q = Simple_queue.enqueue q 1 in 
  let q = Simple_queue.enqueue q 2 in
  let q = Simple_queue.enqueue q 3 in
  let q = dequeue_prt q in
  let q = dequeue_prt q in
  let q = dequeue_prt q in
  let _ = dequeue_prt q in
  ()
;;

let demo_fold () = 
  let q = Simple_queue.empty in
  let q = Simple_queue.enqueue q 1 in 
  let q = Simple_queue.enqueue q 20 in
  let q = Simple_queue.enqueue q 300 in
  printf "fold: %d\n" (Simple_queue.fold q ~init:0 ~f:(+))
;;
