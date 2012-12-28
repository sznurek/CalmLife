open Unix
open Graphics
open Printf

let matrix_iteri f matrix = 
  Array.iteri (fun i v -> Array.iteri (fun j e -> f (i, j) e) v) matrix

let matrix_modify f matrix = 
  matrix_iteri (fun (i, j) e -> matrix.(i).(j) <- f (i, j) e) matrix

let matrix_dim matrix = (Array.length matrix, Array.length matrix.(0))

let matrix_mask matrix mask =
  let (width, height) = matrix_dim matrix in
  let result = Array.make_matrix width height 0.0 in
  let apply_mask (i, j) _ = mask.(i).(j) *. matrix.(i).(j) in
    matrix_modify apply_mask result; result

let matrix_sum matrix = 
  let sum_row row = Array.fold_left (+.) 0.0 row in
  Array.fold_left (fun acc row -> acc +. sum_row row) 0.0 matrix

let random_board (width, height) = 
  let matrix = Array.make_matrix width height 0.0 in 
    matrix_modify (fun _ _ -> if Random.bool() then 1.0 else 0.0) matrix;
    matrix

let fmin (a : float) b = if a < b then a else b

let tor_dist2 w h x1 y1 x2 y2 = 
  let dx = fmin (abs_float (x2 -. x1)) (abs_float (w -. x2 +. x1)) in
  let dy = fmin (abs_float (y2 -. y1)) (abs_float (h -. y2 +. y1)) in
    dx *. dx +. dy *. dy

let circle (w, h) (x, y) r = 
  let dist2 a b = tor_dist2 (float w) (float h) (float x) (float y) a b in
  let r2 = (float r) *. (float r) in
  let matrix = Array.make_matrix w h 0.0 in
    matrix_modify (fun (i, j) _ -> if dist2 (float i) (float j) <= r2 then 1.0 else 0.0) matrix;
    matrix

let sum_with_mask matrix mask = 
  matrix_sum (matrix_mask matrix mask)

let score matrix x y r1 r2 = 
  assert (r2 >= r1);
  let c1 = circle (matrix_dim matrix) (x, y) r1 in
  let c2 = circle (matrix_dim matrix) (x, y) r2 in
  let a1 = matrix_sum c1 in
  let a2 = matrix_sum c2 in
  let s1 = sum_with_mask matrix c1 in
  let s2 = sum_with_mask matrix c2 in
    ((s2 -. s1) /. (a2 -. a1), s1 /. a1)

let alpha = 0.028
let (b1, b2) = (0.248, 0.465)
let (d1, d2) = (0.267, 0.455)
let (r1, r2) = (7, 21)
let sigma1 x a = 1.0 /. (1.0 +. exp (4.0 *. (a -. x) /. alpha))
let sigma2 x a b = (sigma1 x a) *. (1.0 -. (sigma1 x b))
let sigmam x y m = x *. (1.0 -. (sigma1 m 0.5)) +. y *. (sigma1 m 0.5)
let transition n m = sigma2 n (sigmam b1 d1 m) (sigmam b2 d2 m)

let cell_step matrix (x, y) e = 
  let (out_score, in_score) = score matrix x y r1 r2 in
  if transition in_score out_score >= 0.5 then 1.0 else 0.0

let step matrix = 
  let (width, height) = matrix_dim matrix in
  let result = Array.make_matrix width height 0.0 in
    matrix_modify (cell_step matrix) result; result

let print_row row = Array.iter (fun e -> if e <> 0.0 then printf "X" else printf " ") row; print_string "\n"
let print_board board = 
  Array.iter (fun row -> print_row row; print_string "\n") board; 
  printf "********************************\n"; print_newline()

let rec print_steps start n = 
  if n = 0 then () else
    print_board start;
    print_steps (step start) (n - 1)

let (width, height) = (70, 70)
let scale = 5

let rec draw_steps board n = 
  if n = 0 then sleep 1000 else
    print_string "drawing"; print_newline ();
    set_color (rgb 0 0 0);
    fill_rect 0 0 (width * scale) (height * scale);
    set_color (rgb 255 255 255);
    matrix_iteri (fun (x, y) e -> if e > 0.0 then fill_rect (x * scale) (y * scale) scale scale) board;
    draw_steps (step board) (n - 1)

let main () = 
  Random.self_init ();
  let board = random_board (width, height) in
  open_graph (" " ^ (string_of_int (width * scale)) ^ "x" ^ (string_of_int (height * scale)));
  draw_steps board 40
;;

main();;
