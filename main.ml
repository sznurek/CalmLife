open Unix;;
open Graphics;;
open Printf;;
open Matrix;;
open Engine;;

let (width, height) = (150, 150)
let scale = 3

let rec draw_steps board buffer n = 
  if n = 0 then sleep 1000 else
    (print_string "drawing"; print_newline ();
    set_color (rgb 0 0 0);
    fill_rect 0 0 (width * scale) (height * scale);
    set_color (rgb 255 255 255);
    Matrix.matrix_iteri (fun (x, y) e -> if e > 0.0 then fill_rect (x * scale) (y * scale) scale scale) !board;
    Engine.step_inplace board buffer;
    draw_steps board buffer (n - 1))

let main () = 
  Random.self_init ();
  let board = Engine.random_board (width, height) in
  let buffer = Array.make_matrix width height 0.0 in
  open_graph (" " ^ (string_of_int (width * scale)) ^ "x" ^ (string_of_int (height * scale)));
  draw_steps (ref board) (ref buffer) 400
;;

main();;
