module Matrix = struct
  let matrix_iteri f matrix = 
    Array.iteri (fun i v -> Array.iteri (fun j e -> f (i, j) e) v) matrix

  let matrix_modify f matrix = 
    matrix_iteri (fun (i, j) e -> matrix.(i).(j) <- f (i, j) e) matrix

  let matrix_dim matrix = (Array.length matrix, Array.length matrix.(0))

  let matrix_sum matrix = 
    let sum = ref 0.0 in
    matrix_iteri (fun _ v -> sum := !sum +. v) matrix; !sum
end
