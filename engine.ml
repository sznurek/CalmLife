module Engine : sig
  val random_board : int * int -> float array array
  val step : float array array -> float array array
  val step_inplace : float array array ref -> float array array ref -> unit
end = struct
  open Matrix

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

  let alpha = 0.028
  let (b1, b2) = (0.23075, 0.280)
  let (d1, d2) = (0.280, 0.420)
  let (r1, r2) = (2, 3)
  let sigma1 x a = 1.0 /. (1.0 +. exp (4.0 *. (a -. x) /. alpha))
  let sigma2 x a b = (sigma1 x a) *. (1.0 -. (sigma1 x b))
  let sigmam x y m = x *. (1.0 -. (sigma1 m 0.5)) +. y *. (sigma1 m 0.5)
  let transition n m = sigma2 n (sigmam b1 d1 m) (sigmam b2 d2 m)

  let cell_step matrix (x, y) e = 
    let (out_score, in_score) = score matrix x y r1 r2 in
    if transition in_score out_score >= 0.5 then 1.0 else 0.0

  let step matrix = 
    let (width, height) = Matrix.matrix_dim matrix in
    let result = Array.make_matrix width height 0.0 in
      Matrix.matrix_modify (cell_step matrix) result; result

  let swapref a b = 
    let tmp = !a in
      a := !b;
      b := tmp

  let step_inplace matrix buffer = 
    let (width, height) = Matrix.matrix_dim !matrix in
      Matrix.matrix_modify (cell_step !matrix) !buffer; 
      swapref matrix buffer

  let random_board (width, height) = 
    let matrix = Array.make_matrix width height 0.0 in 
      Matrix.matrix_modify (fun _ _ -> if Random.bool() then 1.0 else 0.0) matrix;
      matrix
end
