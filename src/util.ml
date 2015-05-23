(* Util contains basic utility functions *)

let (@@) f x = f x
let (|>) x f = f x
let (<|) f g = fun x -> f @@ g x

let id x = x

let range a b =
  let rec aux x l = if x > b then List.rev l else aux (succ x) (x :: l) in
  aux a []

let rec take l n = match l, n with
  | [], _ | _, 0 -> []
  | hd :: tl, _ -> hd :: take tl (pred n)

let permute arr =
  let len = Array.length arr in
  for i = len - 1 downto 0 do
    let j = Random.int @@ succ i in
    let tmp = arr.(i) in
    arr.(i) <- arr.(j); arr.(j) <- tmp
  done

let count f = List.length <| List.filter f

let outer f l1 l2 = List.map (fun x -> List.map (f x) l2) l1

let matrix_iteri f =
  Array.iteri (fun i r -> Array.iteri (fun j x -> f (i, j) x) r)
