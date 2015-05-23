(* Main loop that launches the minesweeper game *)

let _ = try Mine_control.go 10 10 10 with Graphics.Graphic_failure _ -> ()
