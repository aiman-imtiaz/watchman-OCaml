open TestCases
open GraphicUtil
open Points
open Polygons
open Graphics
open Exercise1
open Exercise5
open WatchmanRoom

let print_route lst =
  (*Prints route as a system output*)
  let l = pointlist_to_intlist (List.map pos_to_square lst) in
  let intlist = List.rev l in
  let rec helper lst =
    match lst with
    | [(a,b)] -> Printf.printf "(%d, %d)]\n" a b
    | (a,b) :: t -> begin
        Printf.printf "(%d, %d);" a b;
        helper t
      end
    | _ -> raise (Failure "print_route error!")
  in
  Printf.printf "[";
  helper intlist

let draw_text s x y = (*Draws screen on the screen*)
  moveto x y;
  draw_string s

let startscreen _ = (*Start screen of the game *)
  let welcome = "Welcome to 'Watchman Maze'!" in
  let goal = "Your goal is to illuminate the all spots in the room" in
  let instruction = "In order to move the watchman:" in
  let a = "Press A to go left" in
  let d = "Press D to go right" in
  let w = "Press W to go up" in
  let s = "Press S to go down" in
  let start = "To start the game please press E" in
  let quit = "You can quit any time by pressing Q" in
  draw_text welcome 25 830;
  draw_text goal 25 815;
  draw_text instruction 25 785;
  draw_text a 25 770;
  draw_text d 25 755;
  draw_text w 25 740;
  draw_text s 25 725;
  draw_text start 25 695;
  draw_text quit 25 680


let tuple_to_string tup = (*Converts tuple into a string *)
  let s = "(" in
  let f = ")" in
  let c = ", " in
  let (x,y) = tup in
  let a = string_of_int x in
  let b = string_of_int y in
  String.concat "" [s;a;c;b;f]

let elem120_string lst = (*Creates a string whose length is less than 120*)
  let rec helper lst acc =
    match lst with
    | [] -> acc
    | a::t -> helper t @@ (tuple_to_string a)::acc
  in
  let l = List.rev @@ helper lst [] in
  String.concat ";" l

let take_first120 lst =
  (*Returns a tuple of two lists: The first is the tuple list that 
    will be converted into a string. The second list is 
    the remaining elements in the path*)
  let count = ref 0 in
  let rec helper lst acc =
    if !count > 100 then begin
        let a = List.rev acc in
        (a, lst)
      end      
    else begin
        match lst with
        | a :: t -> begin
            let len = String.length @@ tuple_to_string a in
            count := !count + len + 1;
            helper t (a::acc)
          end
        | [] -> begin
            let a = List.rev acc in
            (a, lst)
          end 
      end
  in
  helper lst []

let print_120item lst y = (*Prints a string whose length is less than 120*)
  let str = elem120_string lst in
  draw_text str 25 y

let print_route_screen lst = (*Prints route on the screen*)
    let l = pointlist_to_intlist (List.map pos_to_square lst) in
    let intlist = List.rev l in
    let y = ref 740 in
    let rec helper lst =
      match lst with
      | [] -> ()
      | _ -> begin
          let (l1,l2) = take_first120 lst in
          print_120item l1 !y;
          y := !y - 15;
          helper l2
        end 
    in
    helper intlist

let endscreen room = (*End screen of the game *)
  clear_screen ();
  let con = "CONGRATULATIONS!!!" in
  let mis = "You have completed the mission!" in
  let scores = "Your score is:" in
  let score = string_of_int @@ List.length !(room.route) in 
  let paths = "Your path is:" in
  let quit = "In order to quit, please press Q" in
  let start = "If you want to play with a different room please press E" in
  draw_text con 25 830;
  draw_text mis 25 815;
  draw_text scores 25 785;
  draw_text score 25 770;
  draw_text paths 25 755;
  print_route_screen !(room.route);
  draw_text quit 25 50;
  draw_text start 25 20

let rec wait_until_q_pressed room =
  (*The main loop in the game, the keyboard inputs decide the movements*)
  if is_done room then begin
      endscreen room;
      wait_until_start ()
    end
  else begin
      let event = wait_next_event [Key_pressed] in
      if event.key == 'a' || event.key = 'A' then (watchman_left room; draw_room room;
                                                   wait_until_q_pressed room)
      else if event.key == 'w' || event.key = 'W' then (watchman_up room; draw_room room;
                                                        wait_until_q_pressed room)
      else if event.key == 's' || event.key = 'S' then (watchman_down room; draw_room room;
                                                        wait_until_q_pressed room)
      else if event.key == 'd' || event.key = 'D' then (watchman_right room; draw_room room;
                                                        wait_until_q_pressed room)
      else if event.key == 'q' || event.key = 'Q' then exit 0
      else wait_until_q_pressed room
    end
  
and wait_until_start _ =
  (*Creates a random room and waits for the start key *)
  let num = Random.int 10 in
  let tes = [t1;t2;t3;t4;t5;t6;t7;t8;t9;t10] in
  let a = make_room @@ List.nth tes num in
  let event = wait_next_event [Key_pressed] in
  if event.key == 'e' || event.key = 'E' then (clear_screen (); draw_room a; wait_until_q_pressed a)
  else if event.key == 'q' || event.key = 'Q' then exit 0
  else wait_until_start ()

let () = (*To start the game*)
  mk_screen ();
  startscreen ();
  wait_until_start ()
   
