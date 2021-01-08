open Century


let%test "Test equation solver" = solve_unittest solve

                       
let%test "Test equation finder on cenntury problem" =
  let equal_100 = generate_equations_simple 9 100 in
  let not_equal_100 = generate_equations_func 9 (fun x -> x <> 100) in
  
  (* Check have the right number of equations *)
  let l1 = List.length equal_100 in
  let l2 = List.length not_equal_100 in
  assert (l1 + l2 = 6561);
  
  (* Check all values are unique *)
  assert(List.fold_left (fun b eq ->
             b &&
               (List.length (List.filter  (fun a ->
                                 not (cand_equal a eq)) equal_100) =  (l1-1)) &&
                 (List.find_opt (fun a ->
                      cand_equal a eq) not_equal_100 = None))
           true equal_100);

  (* All chosen equations are equal to 100 *)
  assert(List.fold_left (fun b eq ->
             b && solve eq = 100) true equal_100);
  (* None of the other equations equal 100 *)
  assert(List.fold_left (fun b eq ->
             b && solve eq <> 100) true not_equal_100);
  true
