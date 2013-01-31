module Engine : sig
  type config = {
    alpha : float;
    birth_interval : float * float;
    death_interval : float * float;
    radiuses : int * int
  }

  val random_board : int * int -> float array array
  val discrete_step : config -> float array array -> float array array
  val discrete_step_inplace : config -> float array array ref -> float array array ref -> unit

  val continuous_step : config -> float -> float array array -> float array array
  val continuous_step_inplace : config -> float -> float array array ref -> float array array ref -> unit
end = struct
  open Matrix

  type config = {
    alpha : float;
    birth_interval : float * float;
    death_interval : float * float;
    radiuses : int * int
  }

  let rec normalize_position width height (x, y) = 
    if x < 0 then normalize_position width height (x + width, y) else
    if y < 0 then normalize_position width height (x, y + height) else
    (x mod width, y mod height)

  let masked_sum matrix mask (x, y) = 
    let (width, height) = Matrix.matrix_dim matrix in
    let sum = ref 0.0 in
    let add_pos_to_sum (i, j) v = 
      let (i', j') = normalize_position width height (x + i, y + j) in
      sum := matrix.(i').(j') *. v +. !sum
    in Matrix.matrix_iteri add_pos_to_sum mask; !sum

  let circle r = 
    let dist2 a b = (a - r) * (a - r) + (b - r) * (b - r) in
    let r2 = r * r in
    let matrix = Array.make_matrix (2*r + 1) (2*r + 1) 0.0 in
      Matrix.matrix_modify (fun (i, j) _ -> if dist2 i j <= r2 then 1.0 else 0.0) matrix;
      matrix

  let score matrix x y r1 r2 = 
    assert (r2 >= r1);
    let c1 = circle r1 in
    let c2 = circle r2 in
    let a1 = Matrix.matrix_sum c1 in
    let a2 = Matrix.matrix_sum c2 in
    let s1 = masked_sum matrix c1 (x - r1, y - r1) in
    let s2 = masked_sum matrix c2 (x - r2, y - r2) in
      ((s2 -. s1) /. (a2 -. a1), s1 /. a1)

  let sigma1 alpha x a = 1.0 /. (1.0 +. exp (4.0 *. (a -. x) /. alpha))
  let sigma2 alpha x a b = (sigma1 alpha x a) *. (1.0 -. (sigma1 alpha x b))
  let sigmam alpha x y m = x *. (1.0 -. (sigma1 alpha m 0.5)) +. y *. (sigma1 alpha m 0.5)
  let transition config n m = 
    let (b1, b2) = config.birth_interval in
    let (d1, d2) = config.death_interval in
    let alpha = config.alpha in
    sigma2 alpha n (sigmam alpha b1 d1 m) (sigmam alpha b2 d2 m)

  let discrete_cell_step config matrix (x, y) e = 
    let (out_score, in_score) = score matrix x y (fst config.radiuses) (snd config.radiuses) in
    transition config in_score out_score

  let continuous_cell_step config dt matrix (x, y) e = 
    let scaled_score = 2.0 *. (discrete_cell_step config matrix (x, y) e) -. 1.0 in
    let score = scaled_score *. dt +. matrix.(x).(y) in
    if score < 0.0 then 0.0
    else if score > 1.0 then 1.0
    else score

  let swapref a b = 
    let tmp = !a in
      a := !b;
      b := tmp

  let discrete_step config matrix = 
    let (width, height) = Matrix.matrix_dim matrix in
    let result = Array.make_matrix width height 0.0 in
      Matrix.matrix_modify (discrete_cell_step config matrix) result; result

  let discrete_step_inplace config matrix buffer = 
    Matrix.matrix_modify (discrete_cell_step config !matrix) !buffer; 
    swapref matrix buffer

  let continuous_step config dt matrix = 
    let (width, height) = Matrix.matrix_dim matrix in
    let result = Array.make_matrix width height 0.0 in
      Matrix.matrix_modify (continuous_cell_step config dt matrix) result; result

  let continuous_step_inplace config dt matrix buffer = 
    Matrix.matrix_modify (continuous_cell_step config dt !matrix) !buffer;
    swapref matrix buffer

  let random_board (width, height) = 
    let matrix = Array.make_matrix width height 0.0 in 
      Matrix.matrix_modify (fun _ _ -> Random.float 1.0) matrix;
      matrix
end

