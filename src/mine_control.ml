(* MineControl manages the interaction between the user and the graphical
 * and abstract components of the minesweeper game, allowing for the game
 * to progress according to the flow defined here. *)

open Util
open Mine_model
open Mine_graph

(* TODO update state notification (only flagging/mining right now, show
 * game over as well in terminal) *)

(* Returns a clickon instance for either the selection box or the clicked-on
 * square. If mouse was dragged during click over multiple squares or is out
 * of bounds then 'Out' is returned. *)
type clickon = Out | Cell of (int * int) | SelectBox
let locate_click wcf st1 st2 =
  let clickon_off st =
    let x, y = st.Graphics.mouse_x, st.Graphics.mouse_y in
    if x >= wcf.flag_box.x && x <= wcf.flag_box.x + wcf.flag_box.w &&
       y >= wcf.flag_box.y && y <= wcf.flag_box.y + wcf.flag_box.h
    then SelectBox else let cell = wcf.coor (x, y) in
                        if valid wcf.cf cell then Cell cell else Out in
  let r1, r2 = clickon_off st1, clickon_off st2 in
  if r1 = r2 then r1 else Out

type cellcounts = { nhidden:int; nflagged:int }
type minesweeper_state =
  { wcf:window_config; bd:cell array array; cnt:cellcounts }
type gamestages = Mining | Flagging | Terminal
type gamestate = gamestages * minesweeper_state

(* Loops continuously until a key press or full mouse click.
 * Calls appropriate callbacks in each situation, synchronously with the
 * graphics loop. The callback should return the next game state. *)
type loopstate = Continue | Quit
let loop f_key f_mouse gs =
  let rec aux ls gs =
    if ls = Quit then ()
    else begin
      let status = Graphics.wait_next_event
                     [Graphics.Button_down; Graphics.Key_pressed] in
      let ls', gs' =
        if status.Graphics.keypressed then f_key gs status.Graphics.key else
        let status' = Graphics.wait_next_event [Graphics.Button_up] in
        f_mouse gs @@ locate_click (snd gs).wcf status status' in
      aux ls' gs'
      end in
  aux Continue gs



(* TODO move this (?) *)
let create_minesw nb_c nb_r nb_m =
  let nbc = max default_config.ncols nb_c in
  let nbr = max default_config.nrows nb_r in
  let nbm = min (nbc*nbr) (max 1 nb_m) in
  let cf = { ncols = nbc; nrows = nbr; nmines = nbm } in
  (*generate_seed () ; (* TODO random init *)*)
  let wcf = make_wcf cf in
  { wcf = wcf ;
    bd = init_board wcf.cf;
    cnt = { nflagged = 0; nhidden = cf.nrows * cf.ncols - cf.nmines } }

(* get rid of these *)
let d_init nbc nbr nbm =
  let d = create_minesw nbc nbr nbm in
  open_wcf d.wcf; Mining, d
let d_end () = Graphics.close_graph()
let reset game = d_init game.wcf.cf.ncols  game.wcf.cf.nrows game.wcf.cf.nmines

(* Key callback for minesweeper game *)
let d_key gs c = match c with
  | 'q' | 'Q' -> Quit, gs
  | 'r' | 'R' -> Continue, reset @@ snd gs
  | 'f' | 'F' ->
    let stage, game = gs in
    let stage =
      match fst gs with
      | Mining ->
         draw_flag_switch game.wcf true;
         Flagging
      | Flagging ->
         draw_flag_switch game.wcf false;
         Mining
      | Terminal -> Terminal in
    Continue, (stage, game)
  | _ -> Continue, gs
let print_score_game game =
  print_score game.wcf game.cnt.nhidden game.cnt.nflagged

(* Flag a cell, redraw, and return the new minesweeper state. *)
let flag_cell game i j =
  let cell = game.bd.(i).(j) in
  let toggle = { cell with flag = not cell.flag } in
  let delta = if cell.flag then -1 else 1 in
  let game =
    { game with cnt = { game.cnt with nflagged = game.cnt.nflagged + delta } } in
  game.bd.(i).(j) <- toggle;
  draw_cell game.wcf game.bd i j;
  print_score_game game;
  Flagging, game

(* Draw an ending message and window. *)
let ending game str =
  draw_field_end game.wcf game.bd;
  erase_box game.wcf.flag_box;
  draw_string_in_box Center str game.wcf.flag_box Graphics.black;
  Terminal, game

(* Reveal a cell, redrawing and returning the new minesweeper state. *)
let reveal game i j =
  let reveal_cell game (i,j) =
    let cell = game.bd.(i).(j) in
    game.bd.(i).(j) <- { cell with seen = true };
    draw_cell game.wcf game.bd i j;
    { game with cnt = { game.cnt with nhidden = pred game.cnt.nhidden } } in
  let game = List.fold_left reveal_cell game @@
               reachable_cells game.wcf.cf game.bd (i, j) in
  print_score_game game;
  if game.cnt.nhidden = 0 then ending game "WON" else Mining, game

(* Mouse callback for minesweeper game. *)
let d_mouse gs click =
  let stage, game = gs in
  let gs' = match stage, click with
    | Terminal, SelectBox -> reset @@ snd gs
    | Terminal, _ -> gs
    | Mining, Cell (i, j) ->
       let cell = game.bd.(i).(j) in
       if cell.seen || cell.flag then gs
       else if cell.mined then ending game "LOST"
       else reveal game i j
    | Mining, SelectBox ->
       draw_flag_switch game.wcf true;
       Flagging, game
    | Flagging, Cell (i, j) -> flag_cell game i j
    | Flagging, SelectBox ->
       draw_flag_switch game.wcf false;
       Mining, game
    | _, Out -> gs in
  Continue, gs'


let rec go nbc nbr nbm =
  let d = d_init nbc nbr nbm in
  loop d_key d_mouse d;
  d_end ()
