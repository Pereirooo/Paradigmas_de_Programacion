(* pairs.ml *)

(* Definición del tipo int_pair *)
type int_pair = int * int;;

(* Función next: int_pair -> int_pair *)
let next (x, y) =
  if x = 1 then
    if y mod 2 = 0 then (x + 1, y - 1)  
    else (1, y + 1)  
  else
    if y = 1 then 
      if x mod 2 = 0 then (x + 1, 1)  
      else (x - 1, y + 1)  
    else if (x+y) mod 2 = 0 then (x-1, y+1)
    else (x+1, y-1);;  

(* Función steps_from: int_pair -> int -> int_pair *)
let rec steps_from pair n =
  if n = 0 then pair
  else steps_from (next pair) (n - 1);;

(* Función pair: int -> int_pair *)
let rec pair n =
  steps_from (1, 1) (n - 1);;

