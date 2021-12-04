(* let temperature interval maxsteps = maxsteps/interval + 1 *)
let probability current_path_energy neighbour_path_energy temp =
  if neighbour_path_energy >= current_path_energy 
    then Float.exp ((current_path_energy -. neighbour_path_energy) /. temp)
    else 1.

(* get teemperature *)
let temperature current_iterations current_temp interval factor =
  if current_iterations mod interval <> 0 
    then current_temp 
    else current_temp *. factor

let run (s: 'a) energy (next: 'a -> 'a) (t: float) (factor: float) (interval: int) (maxsteps: int) =
  let rec aux acc current temp = 
  (* if acc does not equal maxsteps update values *)
  (* i'm sure there's a better way to do this *)
    if acc <> maxsteps then 
      let current_temp = temperature temp factor acc interval in 
      let neighbour = next current in 
      let current_path_energy = (energy s) in 
      let neighbour_path_energy = (energy neighbour) in 
      let p = probability current_path_energy neighbour_path_energy current_temp in 
      if p >= (Random.float 1.) 
        then aux (acc + 1) neighbour current_temp 
        else aux (acc + 1) current current_temp 
    else ((energy current), current)
  in
  aux 1 s t
