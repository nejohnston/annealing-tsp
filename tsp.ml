open Anneal

(* folds over read_file lines, splitting on " ", filtering "", changing #s to floats *)
let line_fold line =
    let split = (Base.String.split line ~on:(Base.Char.of_string " ")) in
    let filter_split = List.filter (fun x -> x <> "")  split in
    Base.List.fold_right filter_split ~f:(fun x acc -> (Stdlib.float_of_string x)::acc) ~init:[]
    (* List.rev split_int *)

(* reads file *)
let read_file file_name =
  let ic = Stdio.In_channel.create file_name in
    Stdio.In_channel.fold_lines ic ~init:[] ~f:(fun acc x -> (line_fold x)::acc)

(* get item in s at index i *)
(* s is the row got from get_nested_list *)
let rec get_item_at_index (i: int) (am: float list) (count: int) =
  match am with
  | h::_ when i == count -> h
  | _::t -> (get_item_at_index i t (count + 1))
  | [] -> 0.

(* get the adjacency matrix row (i: int) (am: int list list) (count: int) *)
let rec get_nested_list (i: int) (am: float list list) (count: int) = 
  match am with
  | [] -> []
  | h::_ when count == i -> h
  | _::t -> (get_nested_list i t (count + 1))

(* create starting state *)
let make_start_state matrix =
  (* create length of matrix into float *)
  let matrix_len_float = Int.to_float(List.length matrix) in
  (* tail recursive for make_start_state *)
  let rec aux acc new_len =
    if (List.length acc) + 1 == (List.length matrix) 
      then new_len::acc 
      else aux (new_len::acc) (new_len -. 1.)
  in
  aux [] (matrix_len_float -. 1.)

(* creates a random int which does not equal r_int parameter *)
(* used in next function to handle the case where the random ints are equal *)
let rec rand_int r_int length =
  let current_rand_int = Random.int length in
    if current_rand_int == r_int 
      then rand_int current_rand_int length 
      else current_rand_int

(* find next path or "neighbour" *)
let next (s: float list) = 
  (* create two random integers *)
  let list_length = List.length s in
  let rand1 = Random.int list_length in 
  let rand2 = rand_int rand1 list_length in 
  (* pick two elements frorm s,  randomly *)
  let city_to_swap1 = get_item_at_index rand1 s 0 in
  let city_to_swap2 = get_item_at_index rand2 s 0 in 
    let rec next' s' acc count = 
      match s' with
      | [] -> acc
      (* when h does not equal either random city, add element to acc *)
      | h::t when h != city_to_swap1 && h != city_to_swap2 -> next' t (h::acc) (count + 1) 
      (* when h == random city1, add city2 to acc *)
      | h::t when h == city_to_swap1 -> next' t (city_to_swap2::acc) (count + 1) 
      (* when h == random city2, add city1 to acc *)
      | h::t when h == city_to_swap2 -> next' t (city_to_swap1::acc) (count + 1)
    in 
    List.rev(next' s [] 0)

(* find length of path *)
let path_length energy_matrix path =
  let rec aux acc s energy_matrix =
      match s with
      | [] -> acc 
      | [tl] -> 
        (* when there is the last  element we want to get the energy back to home
        we pass tl into get_item_at_index, then we get item at tl and add it to acc *)
        let back_home_e = get_item_at_index (Float.to_int(tl)) (get_nested_list 0 energy_matrix 0) 0 in
        aux (back_home_e+.acc) [] energy_matrix
      | h1::h2::t -> 
      (* same pattern as above except we're finding the next element's energy *)
        let path_energy = get_item_at_index (Float.to_int(h2)) (get_nested_list (Float.to_int(h1)) energy_matrix 0) 0 in
        aux (path_energy+.acc) (h2::t) energy_matrix
  in 
  aux 0. path energy_matrix

let tsp (filename: string) (temp: float) (factor: float) (interval: int) (maxsteps: int) =
  (* create matrix *)
  let matrix = (read_file filename) in
  (* create initial path *)
  let s = make_start_state matrix in
  (* pass in energy *)
  let energy = (path_length matrix) in
  Anneal.run s (energy) next temp factor interval maxsteps 

let () =
    (* creates interval based on system arg *)
    let iterations = Base.Int.of_string (Sys.argv.(2)) in
    let interval = ((iterations/1000) + 1) in
    let a = tsp (Sys.argv.(1)) 100.0 0.99 interval iterations in
    match a with
    | (e, path) -> (Stdio.printf "Path Energy: %f \n" e);
                    Stdio.printf "Path: \n";
                    List.iter (Stdio.printf "%f ") path; 