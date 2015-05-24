(* MineGraph defines the minesweeper game's graphics primitives.
 * TODO add general (i.e., structure, etc.) docs here and in other files. *)

open Mine_model
open Util

(* TODO - these need names... *)
let default_config = { ncols=10; nrows=10; nmines=15 } ;;
let b0 = 3 ;;
let w1 = 15 ;;
let w2 = w1 ;;
let w4 = 20 + 2*b0 ;;
let w3 = w4*default_config.ncols + 2*b0 ;;
let w5 = 40 + 2*b0 ;;
let h1 = w1 ;;
let h2 = 30 ;;
let h3 = w5+20 + 2*b0 ;;
let h4 = h2 ;;
let h5 = 20 + 2*b0 ;;
let h6 = w5 + 2*b0 ;;

(* A box with a relief is one that looks like its interior has been elevated
 * compared to the border. This illusion is accomplished by using a pair of
 * rectangles. *)

(* Identifies where the shadow will be. *)
type relief = Top | Bot | Flat

type box_config =
    (* (x, y) location, (w, h) size, 'bw' border width, 'r' relief
     * {light, shade, col} are various colors for relief illusion.
     * See illustration in 'draw_box' for interaction between fields. *)
    { x:int; y:int; w:int; h:int; bw:int; r:relief
    ; light:Graphics.color
    ; shade:Graphics.color
    ; box_col:Graphics.color}

(* Draws rectangle outline *)
let draw_rect x0 y0 w h =
  let a, b = Graphics.current_point() in
  let x1, y1 = x0 + w, y0 + h in
  Graphics.moveto x0 y0;
  Graphics.lineto x0 y1; Graphics.lineto x1 y1;
  Graphics.lineto x1 y0; Graphics.lineto x0 y0;
  Graphics.moveto a b

(* Draw the outline for a box with color 'col'. *)
let draw_box_outline bcf col =
  Graphics.set_color col;
  draw_rect bcf.x bcf.y bcf.w  bcf.h;;

(* Draw the box with relief. *)
let draw_box bcf =
  (*                                                  p2
   *           +---------------------------------------+  -
   *           | \                 1                 / |  | bw
   *           |   +-------------------------------+   |  -
   *           |   |                         ip2   |   |
   *           |   |                               |   |
   *           |   |                               | 2 |
   *           | 1 |                               |   |
   *           |   |                               |   |
   *           |   | ip1                           |   |
   *           |   +-------------------------------+   |
   *           | /                 2                 \ |
   *        p1 +---------------------------------------+
   *
   * p1 = (x1, y1); p2 = (x2, y2); ip1 = (ix1, iy1); ip2 = (ix2, iy2);
   * 1 = border 1; 2 = border 2
   * *)
  let x1 = bcf.x and y1 = bcf.y in
  let x2 = x1 + bcf.w and y2 = y1 + bcf.h in
  let ix1 = x1 + bcf.bw and ix2 = x2 - bcf.bw in
  let iy1 = y1 + bcf.bw and iy2 = y2 - bcf.bw in
  let border1 g = Graphics.set_color g; Graphics.fill_poly
    [|(x1,y1);(ix1,iy1);(ix2,iy1);(ix2,iy2);(x2,y2);(x2,y1)|] in
  let border2 g = Graphics.set_color g; Graphics.fill_poly
    [|(x1,y1);(ix1,iy1);(ix1,iy2);(ix2,iy2);(x2,y2);(x1,y2)|] in
  Graphics.set_color bcf.box_col;
  begin match bcf.r with
  | Top ->
     Graphics.fill_rect ix1 iy1 (ix2-ix1) (iy2-iy1);
     border1 bcf.light;
     border2 bcf.shade
  | Bot ->
     Graphics.fill_rect ix1 iy1 (ix2-ix1) (iy2-iy1);
     border1 bcf.shade;
     border2 bcf.light
  | Flat ->
     Graphics.fill_rect x1 y1 bcf.w bcf.h end;
  draw_box_outline bcf Graphics.black;;

let erase_box bcf =
  Graphics.set_color bcf.box_col;
  Graphics.fill_rect (bcf.x + bcf.bw) (bcf.y + bcf.bw)
                     (bcf.w - 2 * bcf.bw) (bcf.h - 2 * bcf.bw)

(* Configuration record for entire minesweeper game. *)
type window_config =
    { cf : config
    ; main_box : box_config
    ; field_box : box_config
    ; dialog_box : box_config
    ; d1_box : box_config
    ; d2_box : box_config
    ; flag_box : box_config
    ; flag_text : string * Graphics.color
    ; title_text : string
    ; cell : int * int -> (int * int)
    ; coor : int * int -> (int * int) }

let gray1, gray2, gray3 =
  let set_gray x = Graphics.rgb x x x in
  set_gray 100, set_gray 170, set_gray 240

(* Creates a default-colored box of the specified dimensions *)
let make_box x y w h bw r =
  { x=x; y=y; w=w; h=h; bw=bw; r=r; light=gray1; shade=gray3; box_col=gray2 }

(* Creates a default-sized window configuration given a game config *)
let make_wcf cf =
  let wcols = b0 + cf.ncols * w4 + b0 in
  let hrows = b0 + cf.nrows * h5 + b0 in
  let gw = (b0 + w1 + wcols + w2 + b0) in
  let gh = (b0 + h1 + hrows + h2 + h3 + h4 + b0) in
  let main_box =  make_box 0 0 gw gh b0 Top in
  let field_box = make_box w1 h1 wcols hrows b0 Bot in
  let dialog_box = make_box ((main_box.w - w3) / 2)
                            (b0 + h1 + hrows + h2)
                            w3 h3 b0 Bot in
  let d1_box = make_box (dialog_box.x + b0) (b0 + h1 + hrows + h2)
                        ((w3 - w5) / 2 - 2 * b0) (h3- 2 * b0) 5 Flat in
  let flag_box = make_box (d1_box.x + d1_box.w)
                          (d1_box.y + (h3 - h6) / 2) w5 h6 b0 Top in
  let d2_box = make_box (flag_box.x + flag_box.w)
                        d1_box.y d1_box.w d1_box.h 5 Flat in
  { cf = cf; main_box = main_box; field_box = field_box
  ; dialog_box = dialog_box; d1_box = d1_box; flag_box = flag_box
  ; flag_text = ("OFF", Graphics.blue); d2_box = d2_box
  ; title_text = "Mining"
  ; cell = begin fun (i,j) -> (w1 + b0 + w4 * i, h1 + b0 + h5 * j) end
  ; coor = begin fun (x,y) -> ((x - w1) / w4, (y - h1) / h5) end }

type position = Left | Center | Right;;
let draw_string_in_box pos str bcf col =
  let (w, h) = Graphics.text_size str in
  let ty = bcf.y + (bcf.h-h)/2 in
  begin match pos with
  | Center -> Graphics.moveto (bcf.x + (bcf.w - w) / 2) ty
  | Right -> let tx = bcf.x + bcf.w - w - bcf.bw - 1 in Graphics.moveto tx ty
  | Left -> let tx = bcf.x + bcf.bw + 1 in Graphics.moveto tx ty end;
  Graphics.set_color col;
  Graphics.draw_string str

(* 'cc' is short for cell's box_config
 * Note the following functions draw the various types of different mined
 * cells. *)

let draw_num_cc cc n =
  draw_box cc;
  if n <> 0 then draw_string_in_box Center (string_of_int n) cc Graphics.white
  else ()

let draw_flag_cc cc =
  draw_box cc;
  draw_string_in_box Center "!" cc Graphics.blue

let draw_mine_cc cc =
  draw_box cc;
  Graphics.set_color Graphics.black;
  Graphics.fill_circle (cc.x + cc.w/2) (cc.y + cc.h/2) (cc.h / 3)

(* Does not redraw cell itself, just adds a cross. *)
let draw_cross_cc cc =
  let x, y = cc.x, cc.y in
  let w, h = cc.w, cc.h in
  let a = x + w / 4 and b = x + 3 * w / 4 in
  let c = y + h / 4 and d = y + 3 * h / 4 in
  Graphics.set_color Graphics.red;
  Graphics.set_line_width 3;
  Graphics.moveto a d; Graphics.lineto b c;
  Graphics.moveto a c; Graphics.lineto b d;
  Graphics.set_line_width 1

(* Returns the box_config for a cell at position (i, j) with relief r. *)
let mine_cell_cfg wcf i j r =
  let x, y = wcf.cell (i, j) in
  make_box x y w4 h5 b0 r

(* Draws a cell at position (i, j) according to its state as specified by the
 * parameter board. *)
let draw_cell wcf bd i j =
  let cell = bd.(i).(j) in
  let cc = mine_cell_cfg wcf i j in
  match cell.flag, cell.seen, cell.mined with
  | true, _, _ -> draw_flag_cc @@ cc Top
  | _, false,_ -> draw_box @@ cc Top
  | _, _, true -> draw_mine_cc @@ cc Flat
  | _ -> draw_num_cc (cc Flat) cell.nbrs ;;

(* Draws a cell at position (i, j) according to its state as specified by the
 * parameter board, but under the context of the ended game, where all the
 * positions are in a "revealed" state. *)
let draw_cell_end wcf bd i j =
  let cell = bd.(i).(j) in
  let cc = mine_cell_cfg wcf i j in
  match cell.flag, cell.mined with
  | true, true -> draw_flag_cc @@ cc Top
  | true, false ->
     let cc = cc Flat in draw_num_cc cc cell.nbrs; draw_cross_cc cc
  | false, true -> draw_mine_cc @@ cc Flat
  | false, false -> draw_num_cc (cc Flat) cell.nbrs

(* Redraws sub-components of the graphics utilities *)
let draw_flag_switch wcf =
  draw_box wcf.flag_box;
  let txt, col = wcf.flag_text in
  draw_string_in_box Center txt wcf.flag_box col

let erase_title wcf =
  let w, h = Graphics.text_size wcf.title_text in
  let x = (wcf.main_box.w - w) / 2 in
  let y0 = wcf.dialog_box.y + wcf.dialog_box.h in
  let y = y0 + (wcf.main_box.h - (y0 + h)) / 2 in
  Graphics.set_color gray2;
  Graphics.fill_rect x y (x + w) (y + h)

let draw_title wcf =
  Graphics.set_color Graphics.black;
  let w, h = Graphics.text_size wcf.title_text in
  let x = (wcf.main_box.w - w) / 2 in
  let y0 = wcf.dialog_box.y + wcf.dialog_box.h in
  let y = y0 + (wcf.main_box.h - (y0 + h)) / 2 in
  Graphics.moveto x y; Graphics.draw_string wcf.title_text

(* The following methods update the selection box according to the desired
 * window configuration state. Triggers a redraw. *)
(* TODO: to be completely correct the graphics module shouldn't even distinguish
 * between flagging/mining/terminal states and really there should just be a
 * global function that updates flag/title text generically, but this is
 * more convenient (if less orthogonal in the classic MVC sense) for now. *)
let update_wcf wcf wcf' =
  erase_title wcf;
  draw_flag_switch wcf';
  draw_title wcf';
  wcf'
let set_flagging wcf = update_wcf wcf @@
  { wcf with flag_box = { wcf.flag_box with r = Bot }
           ; flag_text = ("ON", Graphics.red)
           ; title_text = "Flagging" }
let set_mining wcf = update_wcf wcf @@
  { wcf with flag_box = { wcf.flag_box with r = Top }
           ; flag_text = ("OFF", Graphics.blue)
           ; title_text = "Mining" }
let set_terminal wcf str = update_wcf wcf @@
  { wcf with flag_box = { wcf.flag_box with r = Flat }
           ; flag_text = (str, Graphics.black)
           ; title_text = "Game Over" }

let print_score wcf nbcto nbfc =
  erase_box wcf.d1_box;
  draw_string_in_box Center (string_of_int nbcto) wcf.d1_box Graphics.blue;
  erase_box wcf.d2_box;
  draw_string_in_box Center (string_of_int (wcf.cf.nmines - nbfc)) wcf.d2_box @@
    if nbfc > wcf.cf.nmines then Graphics.red else Graphics.blue

(* todo get rid of this *)
let iter_cells cf f =
  for i=0 to cf.ncols-1 do for j=0 to cf.nrows-1 do f (i,j) done done ;;

let draw_field_initial wcf =
  let cc = mine_cell_cfg wcf 0 0 Top in
  let bitmap = draw_box cc; Graphics.get_image cc.x cc.y cc.w cc.h in
  let draw_bitmap (i, j) =
    let x, y = wcf.cell (i, j) in Graphics.draw_image bitmap x y in
  iter_cells wcf.cf draw_bitmap ;;

let draw_field_end wcf bd =
  iter_cells wcf.cf @@ fun (i, j) -> draw_cell_end wcf bd i j

let open_fresh_wcf wcf =
  Graphics.open_graph @@
    " " ^ (string_of_int wcf.main_box.w) ^ "x" ^ (string_of_int wcf.main_box.h);
  draw_box wcf.main_box;
  draw_box wcf.dialog_box;
  draw_flag_switch wcf;
  draw_box wcf.field_box;
  draw_field_initial wcf;
  draw_title wcf;
  print_score wcf (wcf.cf.nrows * wcf.cf.ncols - wcf.cf.nmines) 0
