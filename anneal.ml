(*let build_matrix node_list =*)
(*    let rec build_matrix' acc*)
(*        match node_list with*)
(*        | [] -> acc*)
(*        | h :: t ->*)


(* let print_matrix array = List.iter (Stdio.printf "%d ") array

let line_fold acc line =
    let split = Base.String.split line ~on:(Base.Char.of_string " ") in
      List.fold_right split ~init:[] (fun x acc -> try (Base.Int.of_string x)::acc with Error -> acc) split

let read_file file_name =
  let ic = Stdio.In_channel.create file_name in
    Stdio.In_channel.fold_lines ic ~init:[] ~f:(fun x acc -> line_fold acc line)

let () =
    let a = read_file "data.txt" in
    List.iter (fun x -> Stdio.printf "%d " x) a *)


(*
List.iter and then over that list List.print
*)

(* 
problem: find the shortest path from node a, visiting all nodes, and  back to node a
how to solve
step 1: initialize a random path
step 1.1: calculate energy of path
step 2: swap out two nodes
step 3: compare past shortest path  (lowest Energy), s, with new path (different Energy), s'
step 4: 
 *)

 let energy e_list =
  Base.List.sum e_list

let rec get_item_at_index i s count =
  match s with
  | h::t when i == count -> h
  | _::t -> (get_item_at_index i t (count + 1))
  | [] -> 0

let rec get_matrix_list (i: int) (am: int list list) (count: int) = 
  match am with
  | [] -> []
  | h::t when count == i -> h
  | _::t -> (get_matrix_list i t (count + 1))

let next s = 
  let rand1 = (Random.int (List.length(s) - 1)) in 
  let rand2 = (Random.int (List.length(s) - 1)) in 
  let city_to_swap1 = get_item_at_index rand1 s 0 in
  let city_to_swap2 = get_item_at_index rand2 s 0 in 
    let rec next' s' acc count = 
      match s' with
      | [] -> acc
      | h::t when h <> city_to_swap1 && h <> city_to_swap2 -> next' t (h::acc) (count + 1) 
      | h::t when h == city_to_swap1 -> next' t (city_to_swap2::acc) (count + 1) 
      | h::t when h == city_to_swap2 -> next' t (city_to_swap1::acc) (count + 1)
    in List.rev(next' s [] 0)

(* let run (s: 'a) (energy: 'a -> float) (next: ) t factor interval maxsteps *)
let get_energy s matrix = 
  let rec get_energy' acc start energy_matrix =
    match start with
    | [] -> acc
    | [tl] -> 
      let back_home_e = get_item_at_index tl (get_matrix_list 0 energy_matrix 0) 0 in
      get_energy' (back_home_e::acc) [] energy_matrix
    | h1::h2::t -> 
      let energy = get_item_at_index h2 (get_matrix_list h1 energy_matrix 0) 0 in
      get_energy' (energy::acc) (h2::t) energy_matrix
    in List.rev (get_energy' [] s matrix)

