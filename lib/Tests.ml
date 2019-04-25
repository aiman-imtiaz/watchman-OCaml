(*
Copyright (c) 2019 Ilya Sergey

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
*)


open Points
open TestSegments

(*****************************************)
(*        Tests with points              *)
(*****************************************)

(* Collinearity *)

let%test _ = 
  collinear s3 s4

let%test _ = 
  not @@ collinear s1 s2 &&
  not @@ collinear s3 s2 &&
  not @@ collinear s1 s3 &&
  not @@ collinear s2 s4

(* Point on a segment *)

let%test _ =
  let seg = (r, p) in
  point_on_segment seg t

let%test _ =
  let seg = (r, s) in
  not @@ point_on_segment seg t

(* Random test for point on segment *)
let%test _ =
  for _ = 1 to 100 do
    let seg = gen_random_segment 100. in 
    let p = gen_random_point_on_segment seg in
    assert (point_on_segment seg p)
  done;
  true

(* Intersect non-trivially *)
let%test _ = 
  intersect_as_collinear s3 s4

(* Intersection point *)
let%test _ =
  let p = Week_01.get_exn @@ find_intersection s5 s6 in
  get_x p =~= 0. && get_y p =~= 0.

(*****************************************)
(*        Tests with polygons            *)
(*****************************************)

open Polygons
open TestPolygons

let%test _ = is_convex triangle

let%test _ = is_convex square

let%test _ = is_convex convexPoly2

let%test _ = is_convex convexPoly3

let%test _ = not (is_convex kittyPolygon)
  

(*****************************************)
(*        Testing Convex Hulls           *)
(*****************************************)

open ConvexHulls

let test_random_ch n = 
  let ps = gen_random_points n in 
  let ch = convex_hull ps in
  assert (is_convex ch);
  assert (List.for_all (point_within_polygon ch) ps)


let%test _ = 
  for _ = 0 to 100 do
    test_random_ch 50
  done;
  true
  
let%test _ = 
  let p = 
    [Point (0., 0.); Point (6., 0.); Point (6., 1.); 
     Point (8., 1.); Point (8., 2.); Point (6., 2.); 
     Point (6., 3.); Point (0., 3.)] in
  point_within_polygon p @@ Point (1.,1.)
