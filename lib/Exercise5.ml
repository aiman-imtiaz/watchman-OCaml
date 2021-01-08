open GraphicUtil
open Points
open Polygons
open Graphics
open Exercise1
open WatchmanRoom
open TestCases

let scale = ref 15. (*A variable to adjust the view*)

let scale_point scale p =
  let Point (x,y) = p in
  Point (x *. scale, y *. scale)

let scale_ptlist scale lst =
  List.map (fun (Point (x, y)) ->
      Point (x  *. scale, y *. scale)) lst

let adjust_to_origin pt =
  let Point (x,y) = pt in
  let a = float_of_int @@ fst origin in
  let b = float_of_int @@ snd origin in
  Point(x +. a, y +. b)

let set_scale room = (* Finds a proper scale *)
  let info = room.info in
  let xmin = abs @@ info.(0) in
  let xmax = abs @@ info.(1) in
  let ymin = abs @@ info.(2) in
  let ymax = abs @@ info.(3) in
  if xmin >= xmax && xmin >= ymin && xmin >= ymax then scale := float_of_int @@ 350/xmin
  else if xmax >= xmin && xmax >= ymin && xmax >= ymax then scale := float_of_int @@ 350/xmax
  else if ymin >= xmax && ymin >= xmin && ymin >= ymax then scale := float_of_int @@ 350/ymin
  else scale := float_of_int @@ 350/ymax

let draw_watchman (Point (x,y)) =
  (*Draws the watchman*)
  set_color magenta;
  let scaleint = int_of_float !scale in
  let (a,b) = current_point () in
  let ix = int_of_float x + fst origin in
  let iy = int_of_float y + snd origin in
  moveto ix (iy + scaleint/3);
  lineto ix (iy - scaleint/6);
  lineto (ix - scaleint/5) (iy - scaleint/3); 
  moveto ix (iy - scaleint/6);
  lineto (ix + scaleint/5) (iy - scaleint/3);
  moveto (ix + scaleint/4) iy;
  lineto (ix - scaleint/4) iy;
  fill_circle ix (iy + scaleint/3) (scaleint/8);
  moveto a b;
  set_color black

let color_square square =
  (*Colors a 1 x 1 square whose left bottom corner is the argument square *)
    set_color yellow;
    let Point (x,y) = square in
    fill_rect (int_of_float x) (int_of_float y) (int_of_float !scale) (int_of_float !scale);
    set_color black

let brighten_color room =
  (*Draws the brightened spots*)
  let info = room.info in
  let map = room.brightness in
  let height = Array.length map in
  let width = Array.length map.(0) in
  for i = 0 to height -1 do
    for j = 0 to width-1 do
      if map.(i).(j) = 1
      then color_square @@ adjust_to_origin @@ scale_point !scale @@ index_to_square [|i;j|] info.(0) info.(3);
    done;
  done;
  ()
  
let update_route room =
  (*Updates the route as the watchman moves*)
  let pos = !(room.position) in
  let route = !(room.route) in
  let h = square_to_pos @@ List.hd route in
  if pos <> h
  then room.route := pos::route
  
let draw_room room =
  (* Draws a room*)
  update_route room;
  clear_screen ();
  set_scale room;
  brighten_color room;
  draw_polygon @@ scale_ptlist !scale room.walls;
  draw_watchman @@ scale_point !scale !(room.position)
  
let watchman_up room = (*Moves one unit up *)
  let ptlist = room.walls in
  let Point (x,y) = !(room.position) in
  let newpos = Point (x, y +. 1.) in
  if not (point_within_polygon ptlist newpos)
  then ()
  else begin
      room.position := newpos;
      brighten_map room newpos;
    end

let watchman_down room = (*Moves one unit down *)
  let ptlist = room.walls in
  let Point (x,y) = !(room.position) in
  let newpos = Point (x, y -. 1.) in
  if not (point_within_polygon ptlist newpos)
  then ()
  else begin
      room.position := newpos;
      brighten_map room newpos;
    end

let watchman_right room = (*Moves one unit right *)
  let ptlist = room.walls in
  let Point (x,y) = !(room.position) in
  let newpos = Point (x +. 1., y) in
  if not (point_within_polygon ptlist newpos)
  then ()
  else begin
      room.position := newpos;
      brighten_map room newpos;
    end
  
let watchman_left room = (*Moves one unit left *)
  let ptlist = room.walls in
  let Point (x,y) = !(room.position) in
  let newpos = Point (x -. 1., y) in
  if not (point_within_polygon ptlist newpos)
  then ()
  else begin
      room.position := newpos;
      brighten_map room newpos;
    end
  
let movedecide pos1 pos2 room =
  (*Given two points pos1 and pos2, the function decides 
    the move of the watchman*)
  let (x1,y1) = pos1 in
  let (x2,y2) = pos2 in
  if x1 - x2 = 1 then watchman_left room
  else if x2 - x1 = 1 then watchman_right room
  else if y1 - y2 = 1 then watchman_down room
  else if y2 - y1 = 1 then watchman_up room
  else if ((y2 - y1) = 0) && ((x2 - x1) = 0) then ()
  else begin
      Printf.printf "Initial: %d %d Final: %d %d" x1 y1 x2 y2;
      raise (Failure "movedecide error!")
    end

let usleep s = ignore(Unix.select [] [] [] s) (*Delays for s seconds*)

let visualize_path intlst path =
  (*Visualizing the path to the screen*)
  if List.length path < 2 then raise (Failure "Not a valid route!")
  else begin
      let room = make_room intlst in
      mk_screen ();
      clear_screen ();
      draw_room room;
      usleep 0.15;
      let rec helper path =
        match path with
        |a :: b :: t -> (movedecide a b room;
                         draw_room room;
                         usleep 0.15;
                         helper (b::t))
        |a :: t -> ()
        | _ -> raise (Failure "visualize_path helper error!")
      in
      helper path
    end

let testt1 _ =
  (*Test for maze 1*)
  visualize_path t1 s1

let testt2 _ =
  visualize_path t2 s2

let testt3 _ =
  visualize_path t3 s3

let testt4 _ =
  visualize_path t4 s4

let testt5 _ =
  visualize_path t5 s5

let testt6 _ =
  visualize_path t6 s6

let testt7 _ =
  visualize_path t7 s7

let testt8 _ =
  visualize_path t8 s8

let testt9 _ =
  visualize_path t9 s9

let testt10 _ =
  visualize_path t10 s10
