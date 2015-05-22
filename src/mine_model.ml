(* open Core.Std

let build_counts () =
  In_channel.fold_lines stdin ~init:[] ~f:
    (fun counts line ->
     let count =
       match List.Assoc.find counts line with
       | None -> 0
       | Some x -> x
     in
     List.Assoc.add counts line (count + 1))

let () =
  build_counts ()
  |> List.sort ~cmp:(fun (_,x) (_,y) -> Int.descending x y)
  |> (fun l -> List.take l 10)
  |> List.iter ~f:(fun (line,count) -> printf "%3d: %s\n" count line)

 *)


let (@@) f x = f x
let (|>) x f = f x
let (<|) f g = fun x -> f @@ g x

let id x = x

(*
let range a b =
  let rec aux x l = if x > b then List.rev l else aux (succ x) (x :: l) in
  aux a []

let rec take l n = match l, n with
  | [], _ | _, 0 -> []
  | hd :: tl, _ -> hd :: take tl (pred n) *)

let permute arr =
  let len = Array.length arr in
  for i = len - 1 downto 0 do
    let j = Random.int @@ succ i in
    let tmp = arr.(i) in
    arr.(i) <- arr.(j); arr.(j) <- tmp
  done

let count f = List.length <| List.filter f

let outer f l1 l2 = List.map (fun x -> List.map (f x) l2) l1

let matrix_iteri f m =
  Array.iteri (fun i r -> Array.iteri (fun j x -> f (i, j) x) r) m

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


(* TODO - separate graphics into separate file *)

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
    (* (x, y) location, (w, h) size, 'bw' border width, 'r' relief type,
     * {light, shade, col} are various colors for relief illusion.
     * See illustration in 'draw_box' for interaction between fields. *)
    { x:int; y:int; w:int; h:int; bw:int
    ; mutable r:relief
    ; light:Graphics.color
    ; shade:Graphics.color
    ; box_col:Graphics.color}

(* Draws rectangle outline *)
let draw_rect x0 y0 w h =
  let (a,b) = Graphics.current_point()
  and x1 = x0 + w and y1 = y0 + h in
     Graphics.moveto x0 y0;
     Graphics.lineto x0 y1; Graphics.lineto x1 y1;
     Graphics.lineto x1 y0; Graphics.lineto x0 y0;
     Graphics.moveto a b;;

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
    ; mutable current_box : box_config
    ; cell : int*int -> (int*int)
    ; coor : int*int -> (int*int) }

(* Creates a default-colored box of the specified dimensions *)
let make_box x y w h bw r =
  let set_gray x = Graphics.rgb x x x in
  let gray1, gray2, gray3 = set_gray 100, set_gray 170, set_gray 240 in
  { x=x; y=y; w=w; h=h; bw=bw; r=r; light=gray1; shade=gray3; box_col=gray2 }

(* Creates a default-sized window configuration given a game config *)
let make_wcf cf =
  let wcols = b0 + cf.ncols*w4 + b0 in
  let hrows = b0 + cf.nrows*h5 + b0 in
  let gw = (b0 + w1 + wcols + w2 + b0) in
  let gh = (b0 + h1 + hrows + h2 + h3 + h4 + b0) in
  let main_box =  make_box 0 0 gw gh b0 Top in
  let field_box = make_box w1 h1 wcols hrows b0 Bot in
  let dialog_box = make_box ((main_box.w - w3) / 2)
                            (b0+h1+hrows+h2)
                            w3 h3 b0 Bot in
  let d1_box = make_box (dialog_box.x + b0) (b0 + h1 + hrows + h2)
                        ((w3-w5)/2-(2*b0)) (h3-(2*b0)) 5 Flat in
  let flag_box = make_box (d1_box.x + d1_box.w)
                          (d1_box.y + (h3-h6) / 2) w5 h6 b0 Top in
  let d2_box = make_box (flag_box.x + flag_box.w)
                        d1_box.y d1_box.w d1_box.h 5 Flat in
  let current_box = make_box 0 0 w4 h5 b0 Top in
  { cf = cf; main_box = main_box; field_box = field_box
  ; dialog_box = dialog_box; d1_box = d1_box; flag_box = flag_box
  ; d2_box = d2_box; current_box = current_box
  ; cell = begin fun (i,j) -> (w1 + b0 + w4 * i, h1 + b0 + h5 * j) end
  ; coor = begin fun (x,y) -> ((x - w1) / w4, (y - h1) / h5) end }

let close_ccell wcf i j =
  let x,y = wcf.cell (i,j) in
  wcf.current_box <- {wcf.current_box with x=x; y=y; r=Top}

let open_ccell wcf i j =
  let x,y = wcf.cell (i,j)
  in wcf.current_box <- {wcf.current_box with x=x; y=y; r=Flat}

let draw_closed_cc wcf i j =
  close_ccell wcf i j;
  draw_box wcf.current_box

type position = Left | Center | Right;;

let draw_string_in_box pos str bcf col =
     let (w, h) = Graphics.text_size str in
     let ty = bcf.y + (bcf.h-h)/2 in
     ( match pos with
           Center -> Graphics.moveto (bcf.x + (bcf.w-w)/2) ty
         | Right  -> let tx = bcf.x + bcf.w - w - bcf.bw - 1 in
                     Graphics.moveto tx ty
         | Left   -> let tx = bcf.x + bcf.bw + 1 in Graphics.moveto tx ty  );
     Graphics.set_color col;
     Graphics.draw_string str

let draw_num_cc wcf i j n =
  open_ccell wcf i j ;
  draw_box wcf.current_box ;
  if n<>0 then draw_string_in_box Center (string_of_int n)
                                  wcf.current_box Graphics.white


let draw_mine_cc wcf i j =
  open_ccell wcf i j ;
  let cc = wcf.current_box
  in draw_box wcf.current_box ;
     Graphics.set_color Graphics.black ;
     Graphics.fill_circle (cc.x+cc.w/2) (cc.y+cc.h/2) (cc.h/3) ;;

let draw_flag_cc wcf i j =
  close_ccell wcf i j ;
  draw_box wcf.current_box ;
  draw_string_in_box Center "!" wcf.current_box Graphics.blue ;;

let draw_cross_cc wcf i j =
  let x,y = wcf.cell (i,j)
  and w,h = wcf.current_box.w, wcf.current_box.h in
  let a=x+w/4 and b=x+3*w/4
      and c=y+h/4 and d=y+3*h/4
  in Graphics.set_color Graphics.red ;
     Graphics.set_line_width 3 ;
     Graphics.moveto a d ; Graphics.lineto b c ;
     Graphics.moveto a c ; Graphics.lineto b d ;
     Graphics.set_line_width 1;;

let draw_cell wcf bd i j =
  let cell = bd.(i).(j)
  in match (cell.flag, cell.seen , cell.mined ) with
       ( true ,_,_) -> draw_flag_cc wcf i j
     | (_, false ,_) -> draw_closed_cc wcf i j
     | (_,_, true ) -> draw_mine_cc wcf i j
     | _
       -> draw_num_cc wcf i j cell.nbrs ;;

let draw_cell_end wcf bd i j =
  let cell = bd.(i).(j)
  in match (cell.flag, cell.mined ) with
       ( true , true ) -> draw_flag_cc wcf i j
     | ( true , false ) -> draw_num_cc wcf i j cell.nbrs; draw_cross_cc wcf i j
     | ( false , true ) -> draw_mine_cc wcf i j
     | ( false , false ) -> draw_num_cc wcf i j cell.nbrs

let draw_flag_switch wcf on =
  if on then wcf.flag_box.r <- Bot else wcf.flag_box.r <- Top ;
  draw_box wcf.flag_box ;
  if on then draw_string_in_box Center "ON" wcf.flag_box Graphics.red
  else draw_string_in_box Center "OFF" wcf.flag_box Graphics.blue ;;

let draw_flag_title wcf =
  let m = "Flagging" in
  let w,h = Graphics.text_size m in
  let x = (wcf.main_box.w-w)/2
  and y0 = wcf.dialog_box.y+wcf.dialog_box.h in
  let y = y0+(wcf.main_box.h-(y0+h))/2
  in Graphics.moveto x y ;
     Graphics.draw_string m ;;


let print_score wcf nbcto nbfc =
  erase_box wcf.d1_box ;
  draw_string_in_box Center (string_of_int nbcto) wcf.d1_box Graphics.blue ;
  erase_box wcf.d2_box ;
  draw_string_in_box Center (string_of_int (wcf.cf.nmines-nbfc)) wcf.d2_box
                     ( if nbfc>wcf.cf.nmines then Graphics.red else Graphics.blue ) ;;

(* todo get rid of this *)
let iter_cells cf f =
  for i=0 to cf.ncols-1 do for j=0 to cf.nrows-1 do f (i,j) done done ;;

let draw_field_initial wcf =
  draw_closed_cc wcf 0 0 ;
  let cc = wcf.current_box in
  let bitmap = draw_box cc ; Graphics.get_image cc.x cc.y cc.w cc.h in
  let draw_bitmap (i,j) = let x,y=wcf.cell (i,j)
                          in Graphics.draw_image bitmap x y
  in iter_cells wcf.cf draw_bitmap ;;

let draw_field_end wcf bd =
  iter_cells wcf.cf ( fun (i,j) -> draw_cell_end wcf bd i j) ;;

let open_wcf wcf =
  Graphics.open_graph ( " " ^ (string_of_int wcf.main_box.w) ^ "x" ^
                          (string_of_int wcf.main_box.h)
                      ) ;
  draw_box wcf.main_box ;
  draw_box wcf.dialog_box ;
  draw_flag_switch wcf false ;
  draw_box wcf.field_box ;
  draw_field_initial wcf ;
  draw_flag_title wcf ;
  print_score wcf ((wcf.cf.nrows*wcf.cf.ncols)-wcf.cf.nmines) 0 ;;


(* TODO separate control *)

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
  loop d (d_init d) d_key (d_mouse d) (d_end);;

let _ =go 10 10 10
