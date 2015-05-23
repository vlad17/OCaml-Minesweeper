(* MineModel defines the abstract minesweeper game and allows for
 * modifications to it. *)

open Util

type config = { nrows:int; ncols:int; nmines:int }

type cell = { mined:bool; seen:bool; flag:bool; nbrs:int }

type board = cell array array

(* Converts an index x in the range [0, cfg.nrows * cfg.ncols) to a pair of
 * indices in ranges [0, cfg.nrows) by [0, cfg.ncols) according to the standard
 * left-to-right, top-to-bottom counting method *)
let matrify_idx cfg x = x / cfg.nrows, x mod cfg.ncols

(* Returns m random locations (i,j) on board configured with cfg. *)
let rand_list_mines cfg =
  let ncells = cfg.ncols * cfg.nrows in
  let arr = Array.init ncells id in
  permute arr; Array.sub arr 0 cfg.nmines |> Array.map @@ matrify_idx cfg

(* Checks whether a pair fo indices is in range for a board configured by cfg *)
let valid cfg (i, j) = i >= 0 && j >= 0 && i < cfg.nrows && j < cfg.ncols

(* Returns a list of all the neighbors of a cell. *)
let neighbors cfg (i, j) = List.filter (valid cfg) @@
  [i - 1, j - 1; i - 1, j + 0; i - 1, j + 1;
   i + 0, j - 1;               i + 0, j + 1;
   i + 1, j - 1; i + 1, j + 0; i + 1, j + 1]

(* Initializes a board with mines randomly located, according to cfg *)
let init_board cfg =
  let clear = { mined = false; seen = false; flag = false; nbrs = 0 } in
  let brd = Array.make_matrix cfg.nrows cfg.ncols clear in
  let mine (i, j) =
    brd.(i).(j) <- { mined = true; seen = false; flag = false; nbrs = 0 } in
  Array.iter mine @@ rand_list_mines cfg;
  let update_counts (i, j) x = brd.(i).(j) <-
    { mined = x.mined
    ; seen = x.seen
    ; flag = x.flag
    ; nbrs = count (fun (i, j) -> brd.(i).(j).mined) @@ neighbors cfg (i, j) } in
  matrix_iteri update_counts brd; brd

(* Returns all squares not yet visited, reachable by a square with no nearby
 * mines, and without a flag *)
let reachable_cells cfg brd (i, j) =
  let visited = Array.make_matrix cfg.nrows cfg.ncols false in
  let rec explore (i, j) =
    let cell = brd.(i).(j) in
    if visited.(i).(j) || cell.mined || cell.flag || cell.seen then ()
    else begin
      visited.(i).(j) <- true;
      if cell.nbrs = 0 then List.iter explore @@ neighbors cfg (i, j) else ()
      end in
  explore (i, j);
  let reachable = ref [] in
  let add_reachable x b = if b then reachable := x :: !reachable else () in
  matrix_iteri add_reachable visited; !reachable
