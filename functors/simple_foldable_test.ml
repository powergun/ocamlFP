
open Core

module T = struct
  type 'a t = 'a list * 'a list

  (* NOT WORKING!! *)
  include Simple_foldable
  include Simple_foldable.Extend(T)

  let empty = ([], [])

  let enqueue (in_list, out_list) x =
    (x :: in_list, out_list)
  ;;
  
  let dequeue (in_list, out_list) =
    match out_list with
      | hd :: tl -> Some (hd, (in_list, tl))
      | [] ->
        match List.rev in_list with
          | [] -> None
          | hd :: tl -> Some (hd, ([], tl))
  ;;

  let fold (in_list, out_list) ~init ~f =
    let after_out = List.fold ~init ~f out_list in
    List.fold_right ~init:after_out ~f:(fun x acc -> f acc x) in_list
  ;;

  let from_list xs = 
    let rec aux q = function
      | [] -> q
      | x :: xs -> 
        let q' = enqueue q x in
        aux q' xs
    in
    aux empty xs
  ;;
  
end

let demo () = 
  let q = T.from_list [1; 2; 3; 4; 5; 6] in
  printf "%d\n" (T.exists q ~f:(fun elem -> elem = 6))
;;

