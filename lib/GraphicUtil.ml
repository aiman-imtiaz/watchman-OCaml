open Graphics

let origin = (450, 450)

let go_to_origin _ =
  let x = fst origin in
  let y = snd origin in
  moveto x y;
  set_color black

let mk_screen _ = 
  open_graph " 900x900"
    
let clear_screen _ =
  clear_graph ();;

