open TestCases
open GraphicUtil
open Points
open Polygons
open Graphics
open Week_13_Reachability
open Week_12_Graphs



module WatchmanRoom = struct

  type room = {
      walls : point list;
      brightness : int array array;
      position : point ref;
      info : int array; (*x_min, x_max, y_min, y_max*)
      route : point list ref
    }
            

  let findminmax lst = (*Input is int * int list where first int is x, the other is y*)
    let res = Array.make 4 0 in (*x_min, x_max, y_min, y_max*)
    res.(0) <- fst (List.nth lst 0);
    res.(1) <- fst (List.nth lst 0);
    res.(2) <- snd (List.nth lst 0);
    res.(3) <- snd (List.nth lst 0);
    let rec helper lst =
      match lst with
      | [] -> res
      | (a,b) :: t -> begin
          if a < res.(0) then res.(0) <- a
          else if a > res.(1) then res.(1) <- a;
          if b < res.(2) then res.(2) <- b
          else if b > res.(3) then res.(3) <- b;
          helper t
        end
    in
    helper lst

  let intlist_to_pointlist l =
    List.map (fun (x, y) ->
        Point (float_of_int x, float_of_int y)) l


 (* let pointlist_to_intlist l =
    let fun_to_int elem =
      let Point (x,y) = elem in
      let newx = ref 0 in
      if x>=0. then newx := int_of_float x
      else newx:= (int_of_float x) - 1;
      let newy = ref 0 in
      if y>=0. then newy := int_of_float y
      else newy:= (int_of_float y) - 1;
      (!newx,!newy)
    in
    List.map (fun_to_int) l *)

    let pointlist_to_intlist l =
  List.map (fun (Point (x, y)) ->
      (int_of_float x, int_of_float y)) l

  (*Square indicates the center of 1 x 1 square
    Pos indicates the left bottom corner of 1 x 1 square
    e.g if Pos = (0.5,0.5) then Square = (0.,0.)
    They are both type point*)  
  let pos_to_square p =
    let Point (x,y) = p in
    Point (x  -. 0.5, y -. 0.5)

  let square_to_pos s =
    let Point (x,y) = s in
    Point (x +. 0.5, y +. 0.5)

  (*Index indicates the indeces of a Square in the 
    two-dimensional brightness array*)  
  let square_to_index p xmin ymax =
    let Point (x',y') = p in
    let x = int_of_float x' in
    let y = int_of_float y' in
    [|ymax - y - 1; x - xmin |]

  let index_to_square index xmin ymax =
    let first = index.(0) in
    let second = index.(1) in
    let y = ymax - first - 1 in
    let x = second + xmin in
    Point (float_of_int x, float_of_int y)

  let is_within_bound polygon pos =
    point_within_polygon polygon pos

  let candidates pos =
    (*Finds all the possible sppot in 3 x 3 square whose
      center is pos *)
    let res = Array.make 9 (Point(0.,0.)) in
    let a = ref 0 in
    let Point (x,y) = pos in
    for i = -1 to 1 do
      let i' = float_of_int i in
      for j = -1 to 1 do
        let j' = float_of_int j in
        res.(!a) <- (Point (x +. i', y +. j'));
        a := (!a) + 1
      done;
    done;
    res

  let brighten_map room pos = (*Updates brightness array*)
    let info = room.info in
    let can = candidates pos in
    let walls = room.walls in
    for i=0 to 8 do
      if (is_within_bound walls can.(i))
      then begin
          let t = square_to_index (pos_to_square can.(i)) info.(0) info.(3) in
          let y = t.(0) in
          let x = t.(1) in
          room.brightness.(y).(x) <- 1;
        end
    done;
    ()
 
  let create_brightness_map walls info = (*Initialize brightness array*)
    let width = info.(1) - info.(0) in
    let height = info.(3) - info.(2) in
    let res = (Array.make height [|~-1|]) in
    for i = 0 to height -1 do
      res.(i) <- (Array.make width (-1))
    done;
    for i = 0 to height -1 do
      for j = 0 to width-1 do
        let pos = square_to_pos @@ index_to_square [|i;j|] info.(0) info.(3) in
        if (is_within_bound walls pos)
        then res.(i).(j) <- 0;
      done;
    done;
    res
    
  let make_room lst = (* Initialize a room *)
    let ptlist = intlist_to_pointlist lst in
    let start = Point(0.5, 0.5) in
    if not (point_within_polygon ptlist start)
    then raise (Failure "Square (0,0) is not in the polygon!");
    let inf = findminmax lst in
    let brmap = create_brightness_map ptlist inf in
    let newroom = {
        walls = ptlist;
        brightness = brmap;
        position = ref start;
        info = inf;
        route = ref [Point (0.,0.)]
      }
    in
    brighten_map newroom start; 
    newroom

  let is_done room = (*Checks whether all spots are bright *)
    let res = ref true in
    let map = room.brightness in
    let height = Array.length map in
    let width = Array.length map.(0) in
    let i = ref 0 in
    let j = ref 0 in
    while !res && !i < height do
      while !res && !j < width do
        if map.(!i).(!j) = 0
        then res:= false;
        j := !j + 1
      done;
      i := !i + 1;
      j := 0
    done;
    !res

end

open WatchmanRoom

module WatchmanAuto = struct
  open WatchmanRoom
  open Week_13_Paths
  open Week_01
  open Week_12_Graphs
  open Week_13_Reachability
  open Week_12_BST
  open BinarySearchTree
  open LinkedGraphs
  open NodeTable

  type color = White | Yellow
     

 type rectangle = {
      walls : point list;
      position : point ref;
      info : int array; (*x_min, x_max, y_min, y_max*)
      route : point list ref
    }
     
  let make_rectangle lst =
    let ptlist = intlist_to_pointlist lst in
    let Point(a, b) = List.hd ptlist in
    let start = Point(a +. 0.5, b +. 0.5) in
    let inf = findminmax lst in
    let newrect = {
        walls = ptlist;
        position = ref start;
        info = inf;
        route = ref []
      }
    in
    newrect

  let from_intlist l =
    List.map (fun (Point (x, y)) ->
      (int_of_float x, int_of_float y)) l

  let point_within_rectangle plist p =
    let Point(a, b) = p in
    let x1 = get_x (List.nth plist 0) in
    let x2 = get_x (List.nth plist 1) in
    let y1 = get_y (List.nth plist 0) in
    let y2 = get_y (List.nth plist 3) in
    (a >= x1 && a<= x2) && (b >= y1 && b <= y2)
    
    

   let point_of_int (x, y) =
     if x <= 0
     then Point(float_of_int x, float_of_int y)
     else Point(float_of_int x, float_of_int y)

   let int_of_point (Point(x, y)) =
     (int_of_float x, int_of_float y)



   let h_line_within_polygon ptlist (xk, y) (xl, y) =
     let n = abs_float (float_of_int xl -. float_of_int xk) in
     let rec loop i flag =
       if (i > n) then flag
       else (
         let Point(a, b) = point_of_int (xk, y) in
         if (Polygons.point_within_polygon ptlist (Point (a +. i, b)))
         then (loop (i +. 0.5) true)
         else false)
     in
     loop 0. true

   let v_line_within_polygon ptlist (x, yk) (x, ym) =
     let n = abs_float (float_of_int ym -. float_of_int yk) in
     let rec loop i flag =
       if (i > n) then flag
       else (
         let Point(a, b) = point_of_int (x, yk) in
         if (Polygons.point_within_polygon ptlist (Point (a, b +. i)))
         then (loop (i +. 0.5) true)
         else false)
     in
     loop 0. true

   let find_vk_vl list =
     let y_min = (findminmax list).(2) in
     let filtered = List.filter (function (x, y) -> y = y_min) list in
     let x_min = (findminmax filtered).(0) in
     let filtered2 = List.filter (function (x, y) -> x <> x_min) filtered in
     let next_x_min = (findminmax filtered2).(0) in
     [| (x_min, y_min); (next_x_min, y_min)|]

   let clean list =
     let points = intlist_to_pointlist list in
     let to_remove = List.fold_left (fun acc (p1, p2, p3) ->
                         if direction p1 p2 p3 = 0 then p2 ::acc else acc)
                       [] (all_triples points) in
     let cleaned = List.filter (fun p -> not(List.mem p to_remove)) points in
     from_intlist cleaned

   let  find_rect l =
     let ptlist = intlist_to_pointlist l in
     let rec walk list rect =
       if list = [] then List.rev(!rect) else
         (let reflist = ref list in
          let vk_vl = find_vk_vl !reflist in
          let (xk, yk) = vk_vl.(0) in
          let (xl, yl) = vk_vl.(1) in
          let filtered = List.filter
                           (function (x, y) ->
                              (x >= xk && x <= xl) && y > yk &&
                                h_line_within_polygon ptlist (xk, y) (xl, y) &&
                                  v_line_within_polygon ptlist (xk, yk) (xk, y) &&
                                    v_line_within_polygon ptlist (xl, yl) (xl, y)
                           )
                           !reflist
          in
          begin
            let all_xm = findminmax filtered in
            let x_m = all_xm.(0) in
            let all_ym = List.filter(fun (x, y) -> x = x_m) filtered in
            let ym = (findminmax all_ym).(2) in
            rect := [(xk, yk); (xl, yk); (xl, ym); (xk, ym)] :: !rect;
            reflist := List.filter (function (x, y) -> (x, y) <> (xk, yk)) !reflist;
            reflist := List.filter (function (x, y) -> (x, y) <> (xl, yl)) !reflist;
            (if List.mem (xl, ym) !reflist then
               reflist := List.filter (function (x, y) -> (x, y) <> (xl, ym)) !reflist
             else reflist := (xl, ym) :: !reflist);
            (if List.mem (xk, ym) !reflist then
               reflist := List.filter (function (x, y) -> (x, y) <> (xk, ym)) !reflist
             else reflist := (xk, ym) :: !reflist);
          end;
          walk !reflist rect)
     in
     walk (clean l) (ref [])



   let watchman_up_rect rect =
     let Point (x,y) = !(rect.position) in
     let newpos = Point (x, y +. 1.) in
     begin
       rect.route := newpos :: !(rect.route);
       rect.position := newpos;
     end

   let watchman_down_rect rect =
     let Point (x,y) = !(rect.position) in
     let newpos = Point (x, y -. 1.) in
     begin
       rect.route := newpos :: !(rect.route);
       rect.position := newpos;
     end

   let watchman_right_rect rect =
     let Point (x,y) = !(rect.position) in
     let newpos = Point (x +. 1., y) in
     rect.route := newpos :: !(rect.route);
     rect.position := newpos
  
   let watchman_left_rect rect =
     let Point (x,y) = !(rect.position) in
     let newpos = Point (x -. 1., y) in
     begin
       rect.route := newpos :: !(rect.route);
       rect.position := newpos;
     end


   let steps_up rect =
     let ptlist = rect.walls in
     let Point (x, y) = !(rect.position) in
     if not (point_within_rectangle ptlist (Point(x, y +. 1.))) then 0
     else (if point_within_rectangle ptlist (Point(x, y +. 2.))
           then 2 else 1)

   let steps_down rect =
     let ptlist = rect.walls in
     let Point (x, y) = !(rect.position) in
     if not (point_within_rectangle ptlist (Point(x, y -. 1.))) then 0
     else (if point_within_rectangle ptlist (Point(x, y -. 2.))
           then 2 else 1)

   let steps_right rect =
     let ptlist = rect.walls in
     let Point (x, y) = !(rect.position) in
     if not (point_within_rectangle ptlist (Point(x +. 1., y))) then 0
     else (if point_within_rectangle ptlist (Point(x +. 2., y))
           then 2 else 1)

   let steps_left rect =
     let ptlist = rect.walls in
     let Point (x, y) = !(rect.position) in
     if not (point_within_rectangle ptlist (Point(x -. 1., y))) then 0
     else (if point_within_rectangle ptlist (Point(x -. 2., y))
           then 2 else 1)

          
   let sweep_rectangle rect' =
     let info = rect'.info in
     let width = info.(1) - info.(0) in
     let Point(a, b) = List.hd rect'.walls in
     let start = Point(a +. 0.5, b +. 0.5) in
     rect'.route := [start];

     let rec walk rect dir =
       if steps_up rect > 0 then
         watchman_up_rect rect;
       if width > 2 then
         begin
           if (!dir = 1) then
             (for i = 1 to (width - 2) do
             watchman_right_rect rect
              done; dir:= 0)
           else
             (for i = 1 to (width - 2) do
             watchman_left_rect rect
              done; dir:= 1)
         end;
       if (steps_up rect) < 2 then !(rect.route)
       else (watchman_up_rect rect;
       if steps_up rect > 0 then
         watchman_up_rect rect;             
             walk rect dir)
       
     in
     walk rect' (ref 1) 

   let step_dy r1 intersection =
     let (_, dy) = intersection in
     let n = abs (int_of_float dy) in
     if dy > 0.
     then
       for i = 1 to n do
         watchman_up_rect r1
       done
     else
       for i = 1 to n do
         watchman_down_rect r1
       done

   let step_dx r1 intersection =
     let (dx, _) = intersection in
     let n = abs (int_of_float dx) in
     if dx > 0.
     then
       for i = 1 to n do
         watchman_right_rect r1
       done
     else
       for i = 1 to n do
         watchman_left_rect r1
       done     


   let go_within r1 =
     (r1.route) :=  [];
     let Point(x1, y1) = pos_to_square !(r1.position) in
     let Point(x2, y2) = List.hd (r1.walls) in
     let dx_dy = (x2 -. x1, y2 -. y1) in
     step_dx r1 dx_dy;
     step_dy r1 dx_dy(*;
     (match !(r1.route) with
      | h:: t ->(r1.route) := t
      | [] ->  (r1.route) := !(r1.route) ) *)

     let go_to_connected r1 r2 =
     (assert (polygons_touch_or_intersect (r1.walls) (r2.walls)));
     r1.route := [];
     if (r1.walls) = (r2.walls) then !(r1.route) else (
     let Point(x, y) = pos_to_square !(r1.position) in
     let e1 = edges r1.walls in
     let e2 = edges r2.walls in
     let intersection = ref (0., 0.) in
     let intersect_list = ref [] in
     List.iter (fun line1 -> List.iter(fun line2 ->
                                 if segments_intersect line1 line2
                                 then intersect_list := List.sort_uniq compare
                                                          (Week_01.get_exn
                                                             (find_intersection line1 line2)
                                                           :: !intersect_list)) e2) e1;
     (match !intersect_list with
      | a :: b :: _ -> let x1 = get_x a in
                        let x2 = get_x b in
                        let y1 = get_y a in
                        let y2 = get_y b in
                        if x1 = x2 then
                          begin
                            (if y1 < y2
                             then  intersection := (x1 -. x, y1 -. y)
                             else intersection := (x1 -. x, y2 -. y) );
                            step_dy r1 !intersection;
                            step_dx r1 !intersection
                          end
                        else if y1 = y2 then
                          begin
                            (if x1 < x2
                             then
                               intersection := (x1 -. x, y1 -. y)
                             else
                                intersection :=(x2 -. x , y1 -. y));
                            step_dx r1 !intersection;
                            step_dy r1 !intersection
                          end 
                         
      | _ -> () );
    (match !(r1.route) with
     | h:: t -> r2.position := h
     | [] -> r2.position := !(r1.position) );
    (match !(r2.position) with
     | p -> let Point(x, y) = pos_to_square p in
            if x = float_of_int(r2.info.(1)) then
              watchman_left_rect r1;
            if y = float_of_int(r2.info.(3)) then
              watchman_down_rect r1;
            r2.position := !(r1.position) );
    !(r1.route) )





    let graph_of_room roomintlist =
     let rectangles = List.map make_rectangle (find_rect roomintlist) in
     let origin_rect = ref None in
     let graph = LinkedGraphs.mk_graph() in
     List.iter(fun rect -> if point_within_rectangle (rect.walls)
                                (Point(0.5, 0.5))
                           then origin_rect := Some rect else ()) rectangles;
     LinkedGraphs.add_node graph (Week_01.get_exn !origin_rect);
     let filtered = List.filter(fun rect ->
                        rect <> (Week_01.get_exn !origin_rect)) rectangles in
     List.iter (fun rect -> LinkedGraphs.add_node graph rect) filtered;
     let n = !(graph.next_node_id) in
     for i = 0 to (n-1) do
       for j = 0 to (n-1) do
         if i <> j then
           begin
           let r1 = get_linked_node_payload graph i in
           let r2 = get_linked_node_payload graph j in
           if (polygons_touch_or_intersect r1.walls r2.walls)
           then LinkedGraphs.add_edge graph i j
           end
       done
     done;
     graph



    let list_kill n list =
      let rec helper i list' acc =
        if i >= n then acc
        else match list' with
             | h :: t -> helper (i + 1) t (h::acc)
             | [] -> list'
      in
      helper 0 list []

    let sweep_room list =
     let graph = graph_of_room list in
     let path = ref [] in
     let cut_off = ref 0 in

     let dfs g =
       (
       let color_map = mk_new_table (v_size g) in
       let sweep_count = ref 0 in
       let all_nodes = get_nodes g in

       let origin_rect = (get_linked_node_payload g 0) in
       origin_rect.position:= Point (0.5, 0.5);

       List.iter (fun n -> insert color_map n White) all_nodes;

       let rec dfs_visit u =
         let u_rect = get_linked_node_payload g u in
         go_within u_rect; 
         let path3 = u_rect.route in
         path := List.append !path3 !path;
         let path1 = sweep_rectangle u_rect in
         path := List.append path1 !path;
         sweep_count := !sweep_count + 1;
         if (!sweep_count = (v_size g)) then
           cut_off := List.length (!path);
         insert color_map u Yellow;
         get_succ g u |> List.iter (fun v ->
                             let v_rect = get_linked_node_payload g v in
                             let v_color = get_exn @@ get color_map v in
                             if v_color = White
                             then begin
                                 let path2 = go_to_connected u_rect v_rect in
                                 path := List.append path2 !path;
                                 dfs_visit v;
                                 let path4 = go_to_connected v_rect u_rect in
                                 path := List.append path4 !path
                               end else ())
       in
       dfs_visit 0)
       in
       dfs graph;
       let sol_only = list_kill !cut_off (List.rev !path) in
       let pos_list = List.rev (sol_only) in
       let sq_list = List.map (fun p -> pos_to_square p) pos_list in
       let int_list = List.map (fun p -> int_of_point p) sq_list in
       if (List.hd int_list) <> (0, 0) then ((0, 0) :: int_list)
      else int_list


end

open WatchmanAuto                        
