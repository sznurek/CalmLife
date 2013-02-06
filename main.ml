open Graphics;;
open Matrix;;
open Engine;;
open Config;;

let (width, height) = (200, 200)
let scale = 2

let set_cell_color cell = 
  let value = int_of_float (cell *. 255.0)
  in set_color (rgb value value value)

let rec draw_steps config board buffer = 
  set_color (rgb 0 0 0);
  fill_rect 0 0 (width * config.scale) (height * config.scale);
  set_color (rgb 255 255 255);
  Matrix.matrix_iteri (fun (x, y) e -> set_cell_color e; fill_rect 
                      (x * config.scale) (y * config.scale) config.scale config.scale) !board;
  Engine.continuous_step_inplace config.engine_config config.dt board buffer;
  draw_steps config board buffer

let main () = 
  Random.self_init ();
  let (width, height) = config.window_dimensions in
  let scale = config.scale in
  let board = Engine.random_board (width, height) in
  let buffer = Array.make_matrix width height 0.0 in
  open_graph (" " ^ (string_of_int (width * scale)) ^ "x" ^ (string_of_int (height * scale)));
  draw_steps config (ref board) (ref buffer)
;;

main();;
