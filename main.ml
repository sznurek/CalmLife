open Graphics;;
open Matrix;;
open Engine;;

type life_config = {
  engine_config : Engine.config;
  dt : float;
  window_dimensions : int * int;
  window_scale : int
}

let read_file chan = 
  let lines = ref [] in
  try
    while true; do
      lines := input_line chan :: !lines
    done; [];
  with End_of_file ->
    List.rev !lines

let read_config () =
  let lines = read_file stdin in
  lines

let (width, height) = (200, 200)
let scale = 2

let set_cell_color cell = 
  let value = int_of_float (cell *. 255.0)
  in set_color (rgb value value value)

let rec draw_steps config board buffer = 
  set_color (rgb 0 0 0);
  fill_rect 0 0 (width * scale) (height * scale);
  set_color (rgb 255 255 255);
  Matrix.matrix_iteri (fun (x, y) e -> set_cell_color e; fill_rect (x * scale) (y * scale) scale scale) !board;
  Engine.continuous_step_inplace config 0.2 board buffer;
  draw_steps config board buffer

let main () = 
  Random.self_init ();
  let config = {
    Engine.alpha = 0.05;
    Engine.birth_interval = (0.280, 0.365);
    Engine.death_interval = (0.290, 0.445);
    Engine.radiuses = (4, 12)
  } in
  let board = Engine.random_board (width, height) in
  let buffer = Array.make_matrix width height 0.0 in
  open_graph (" " ^ (string_of_int (width * scale)) ^ "x" ^ (string_of_int (height * scale)));
  draw_steps config (ref board) (ref buffer)
;;

main();;
