open TestCases
open GraphicUtil
open Points
open Polygons
open Graphics
open Exercise5
open Exercise1
open WatchmanRoom
open WatchmanAuto
   
let rec go_up lst =
  let len = 1. +. (float_of_int (Random.int 10)) in
  let newy = 1. *. len in
  let Point (x,y) = List.hd lst in
  let newpoint = Point (x,y +. newy) in
  newpoint::lst

let rec go_down lst =
  let len = 1. +. (float_of_int (Random.int 10)) in
  let newy = -1. *. len in
  let Point (x,y) = List.hd lst in
  let newpoint = Point (x,y +. newy) in
  newpoint::lst

let rec go_right lst =
  let len = 1. +. (float_of_int (Random.int 10)) in
  let newx = 1. *. len in
  let Point (x,y) = List.hd lst in
  let newpoint = Point (x +. newx,y) in
  newpoint::lst

let rec go_left lst =
  let len = 1. +. (float_of_int (Random.int 10)) in
  let newx = -1. *. len in
  let Point (x,y) = List.hd lst in
  let newpoint = Point (x +. newx,y) in
  newpoint::lst

let rec go_leftend lst =
  let Point (x,y) = List.hd lst in
  let newpoint = Point (5.,y) in
  let newpoint2 = Point (5.,31.) in
  let newpoint3 = Point (-31.,31.) in
  newpoint3::newpoint2::newpoint::lst

let downadjust lst =
  let Point (x,y) = List.hd lst in
  let newpoint = Point (x,0.) in
  newpoint::lst

let rec go_rightend lst =
  let Point (x,y) = List.hd lst in
  let newpoint = Point (-5.,y) in
  let newpoint2 = Point (-5.,-31.) in
  let newpoint3 = Point (31.,-31.) in
  newpoint3::newpoint2::newpoint::lst

let upadjust lst =
  let Point (x,y) = List.hd lst in
  let newpoint = Point (0.,y) in
  newpoint::lst

let randomroom _ = (* Creates a random room*)
  let room = ref [Point (0.,0.)] in
  for i = 0 to 2 do
    room := go_right !room;
    room := go_up !room;
  done;
  room := go_leftend !room;
  for i = 0 to 2 do
    room := go_down !room;
    room := go_right !room;
  done;
  room := downadjust !room;
  for i = 0 to 2 do
    room := go_left !room;
    room := go_down !room;
  done;
  room := go_rightend !room;
  for i = 0 to 2 do
    room := go_up !room;
    room := go_left !room;
  done;
  room := upadjust !room;
  List.rev @@ pointlist_to_intlist (!room)

let drawrandomroom _ = (*Draws a random room *)
  clear_screen ();
  draw_polygon @@ scale_ptlist 10. @@ intlist_to_pointlist @@ randomroom ()

let try_path intlst path =
  (*Trying the path to check whether it works or not*)
  let room = make_room intlst in
  if List.length path < 2 then raise (Failure "Not a valid route!")
  else begin
      let rec helper path =
        match path with
        |a :: b :: t -> (movedecide a b room;
                         helper (b::t))
        |a :: t -> ()
        | _ -> raise (Failure "try_path helper error!")
      in
      helper path
    end;
  is_done room

let%test "10cases" =
  let tes = [t1;t2;t3;t4;t5;t6;t7;t8;t9;t10] in
  for i = 0 to 9 do
    let room = List.nth tes i in
    let path = sweep_room room in
    assert(try_path room path)
  done; 
  true
    

let%test "Test path" =
  for i = 0 to 50 do
    let room = randomroom () in
    let path = sweep_room room in
    assert(try_path room path)
  done; 
  true


