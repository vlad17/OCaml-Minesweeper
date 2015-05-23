(* MineControl manages the interaction between the user and the graphical
 * and abstract components of the minesweeper game, allowing for the game
 * to progress according to the flow defined here. *)

open Util
open Mine_model
open Mine_graph

exception End

type clickon = Out | Cell of (int*int) | SelectBox ;;

let locate_click wcf st1 st2 =
  let clickon_of st =
    let x = st.Graphics.mouse_x and y = st.Graphics.mouse_y
    in if x>=wcf.flag_box.x && x<=wcf.flag_box.x+wcf.flag_box.w &&
            y>=wcf.flag_box.y && y<=wcf.flag_box.y+wcf.flag_box.h
       then SelectBox
       else let (x2,y2) = wcf.coor (x,y)
            in if x2>=0 && x2<wcf.cf.ncols && y2>=0 && y2<wcf.cf.nrows
               then Cell (x2,y2) else Out
  in
  let r1=clickon_of st1 and r2=clickon_of st2
  in if r1=r2 then r1 else Out ;;

type minesw_cf =
    { wcf : window_config; bd : cell array array;
      mutable nb_flagged_cells : int;
      mutable nb_hidden_cells : int;
      mutable flag_switch_on : bool } ;;

let loop d f_init f_key f_mouse f_end =
  f_init ();
  try
    while true do
      let st = Graphics.wait_next_event
                 [Graphics.Button_down;Graphics.Key_pressed]
      in if st.Graphics.keypressed then f_key st.Graphics.key
         else let st2 = Graphics.wait_next_event [Graphics.Button_up]
              in f_mouse (locate_click d.wcf st st2)
    done
  with End -> f_end ()


let d_init d () = open_wcf d.wcf
let d_end () = Graphics.close_graph()
let d_key c = if c='q' || c='Q' then raise End;;

let flag_cell d i j =
  let cell = d.bd.(i).(j) in
  let toggle = { mined = cell.mined; seen = cell.seen; flag = not cell.flag;
                 nbrs = cell.nbrs } in
  if cell.flag
  then ( d.nb_flagged_cells <- d.nb_flagged_cells -1;
         d.bd.(i).(j) <- toggle )
  else ( d.nb_flagged_cells <- d.nb_flagged_cells +1;
         d.bd.(i).(j) <- toggle );
  draw_cell d.wcf d.bd i j;
  print_score d.wcf d.nb_hidden_cells d.nb_flagged_cells;;

let ending d str =
  draw_field_end d.wcf d.bd;
  erase_box d.wcf.flag_box;
  draw_string_in_box Center str d.wcf.flag_box Graphics.black;
  ignore(Graphics.wait_next_event
           [Graphics.Button_down;Graphics.Key_pressed]);
  raise End;;

let reveal d i j =
  let reveal_cell (i,j) =
    let cell = d.bd.(i).(j) in
    d.bd.(i).(j) <- { mined = cell.mined; seen = true; flag = cell.flag; nbrs = cell.nbrs };
    draw_cell d.wcf d.bd i j;
    d.nb_hidden_cells <- d.nb_hidden_cells -1
  in
  List.iter reveal_cell (reachable_cells d.wcf.cf d.bd  (i,j));
  print_score d.wcf d.nb_hidden_cells d.nb_flagged_cells;
  if d.nb_hidden_cells = 0 then ending d "WON";;

let d_mouse d click = match click with
    Cell (i,j) ->
    if d.bd.(i).(j).seen then ()
    else if d.flag_switch_on then flag_cell d i j
    else if d.bd.(i).(j).flag then ()
    else if d.bd.(i).(j).mined then ending d "LOST"
    else reveal d i j
  | SelectBox ->
     d.flag_switch_on <- not d.flag_switch_on;
     draw_flag_switch d.wcf d.flag_switch_on
  | Out -> () ;;

let create_minesw nb_c nb_r nb_m =
  let nbc = max default_config.ncols nb_c
  and nbr = max default_config.nrows nb_r in
  let nbm = min (nbc*nbr) (max 1 nb_m) in
  let cf = { ncols=nbc ; nrows=nbr ; nmines=nbm } in
  (*generate_seed () ; (* RANDOM Init *)*)
  let wcf = make_wcf cf in
  { wcf = wcf ;
    bd = init_board wcf.cf;
    nb_flagged_cells = 0;
    nb_hidden_cells = cf.nrows*cf.ncols-cf.nmines;
    flag_switch_on = false } ;;


let go nbc nbr nbm =
  let d = create_minesw nbc nbr nbm in
  try loop d (d_init d) d_key (d_mouse d) (d_end) with End -> ()
