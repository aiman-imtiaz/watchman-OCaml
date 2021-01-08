


(***********************************************)
(******         A CENTURY PROBLEM         ******)
(***********************************************)




(******          TYPE DEFINITIONS         ******)

(* Define a type to represent operations (or lack thereof) *)
type operator = Add | Mul | Zip | Nil
                                         
let do_arith op arg1 arg2 =
  match op with
  | Add -> arg1 + arg2
  | Mul -> arg1 * arg2
  | Zip -> arg1*10 + arg2
  | Nil -> raise (Failure "Full operations not filled out")
                
let print_op op =
  match op with
  | Add -> " + "
  | Mul -> " x "
  | Zip -> ""
  | Nil -> raise (Failure "Full operations not filled out")

         
(* Define a type to represent candidate equations *)
type candidate = {ops : operator array;
                  size : int;
                 }

let add_op c idx op =
  c.ops.(idx) <- op
                    
let get_op c idx =
  c.ops.(idx)
               
let mk_cand s = {
    ops = Array.make (s-1) Nil;
    size = s-1;
  }
              
let copy_cand c = {
    ops = Array.copy c.ops;
    size = c.size
  }
let cand_equal c1 c2 =
  c1.ops = c2.ops && c1.size = c2.size




(******        SOLVING AN EQUATION        ******)

(* Unittest for a solver *)
let solve_unittest solver =
  (solver {ops = [|Add;Zip;Mul;Mul;Zip;Mul;Zip|];
    size = 7} = 401857) && 
  (solver {ops = [|Zip;Mul;Zip;Add;Zip;Mul;Add|];
    size = 7} = 808) && 
  (solver {ops =[|Mul;Zip;Mul;Zip;Mul;Zip;|];
    size = 6} = 69345) && 
  (solver {ops = [|Mul;Zip;Add;Mul;Zip;Zip;Mul;Add|];
    size = 8} = 18176) && 
  (solver {ops =  [|Zip;Zip;Zip;Zip;Add;Mul;Mul;Add|];
           size = 8} = 12690)

(* Solver - with verbose set to true, will 
   print all the steps. Quite helpful. *)
let solve_verbose verbose c =
  let result = ref 0 in
  let op_out = ref Add in
  let op_in = ref Add in
  let rem_out = ref 0 in
  let rem_in = ref 1 in
  let update_inner op i =
    rem_out := do_arith !op_in !rem_out !rem_in;
    op_in := op;
    rem_in := i
  in
  let update_outer op i =
    update_inner Add i;
    result := do_arith !op_out !result !rem_out;
    op_out := op;
    rem_out := 0;
  in
  for j = 2 to c.size + 1 do
      if verbose then
        Printf.printf " %d %s %d %s %d \n"
          !result (print_op !op_out) !rem_out (print_op !op_in) !rem_in
      else (); 
    match get_op c (j - 2) with
    | Zip -> rem_in := do_arith Zip !rem_in j
    | Add -> update_outer Add j
    | Mul -> update_inner Mul j
    | Nil -> raise (Failure "Full operations not filled out")
  done;
  update_outer Nil c.size;
  !result

let solve = solve_verbose false;;




(******        PRETTY PRINTER        ******)

(* 
   Printer - first calculates and prints the 
   soltion, then the equation. Uses the function
   for printing operations defined earlier in type
   section.
*)
let print_eq c =
  let solution = solve c in
  let rec build_string i str =
    if i >= c.size then str
    else let new_str = Printf.sprintf "%s%s%d" str (print_op c.ops.(i)) (i+2) in
         build_string (i+1) new_str
  in
  let eq = build_string 0 "1" in
  Printf.printf "%d = %s\n" solution eq
  


  
(******  GENERATING SUCCESSFUL CANDIDATES  ******)

(* 
   Generate all possible equations that use the first 
   len ordered natural numbers. Keep all where the 
   solution returns true when applied to the matches
   function.
*)
let generate_equations_func len matches =
  let cand = mk_cand len in
  let successes = ref [] in
  let rec build_eq i result op_out rem_out op_in rem_in =
    if i > len 
    then begin
        let solution = do_arith op_out result (do_arith op_in rem_out rem_in) in
        if matches solution
        then successes := (copy_cand cand) :: (!successes)
        else ()
      end
    else begin
        add_op cand (i-2) Add;
        build_eq (i+1) (do_arith op_out result
                          (do_arith op_in rem_out rem_in)) Add 0 Add i;
       
        add_op cand (i-2) Mul;
        build_eq (i+1) result op_out (do_arith op_in rem_out rem_in) Mul i;
        
        add_op cand (i-2) Zip;
        build_eq (i+1) result op_out rem_out op_in (do_arith Zip rem_in i)
      end
  in
  build_eq 2 0 Add 0 Add 1;
  !successes

(* Simpler version - returns all solutions which resolve
   to target, instead of matching a function. *)
let generate_equations_simple len target =
  generate_equations_func len (fun x -> x = target)

(* Print all answers to the century problem *)
let print_century_expressions _ =
  let ls = generate_equations_simple 9 100 in
  List.iter (fun c -> print_eq c) ls


