(*val hd: 'a list -> 'a*)
let hd lista =
  match lista with
  | [] -> failwith "La lista está vacía, no hay cabeza."
  | x :: _ -> x;;

(*val tl: 'a list -> 'a list*)
let tl lista =
  match lista with
  | [] -> failwith "La lista está vacía, no hay cola."
  | _ :: resto -> resto;;

(*val length: 'a list -> int*)
let rec length lista =
  match lista with
  | [] -> 0
  | _ :: resto -> 1 + length resto;;
  
(*val compare_lengths : 'a list -> 'b list -> int*)
let compare_lengths lista1 lista2 =
  let rec length lista =
    match lista with
    | [] -> 0
    | _ :: resto -> 1 + length resto
  in
  compare (length lista1) (length lista2);;

  
(*val compare_length_with : 'a list -> int -> int*)
let compare_length_with lista longitud_comparar =
  let rec length lista =
    match lista with
    | [] -> 0
    | _ :: resto -> 1 + length resto
  in
  compare (length lista) longitud_comparar;;


(*val init : int -> (int -> 'a) -> 'a list*)
let init len f =
  if len < 0 then
    invalid_arg "init: len must be non-negative"
  else
    let rec aux i acc =
      if i = len then
        List.rev acc
      else
        aux (i + 1) (f i :: acc)
    in
    aux 0 [];;



(*val nth : 'a list -> int -> 'a*)
let rec nth list x = match x,list with 
	x,_ when x < 0 -> raise (Invalid_argument "nth") 
	| _,[] -> raise (Invalid_argument "nth")
	| 0,h::_ -> h
	| x,_::t -> nth t (x - 1);;


(*val append : 'a list -> 'a list -> 'a list*)
let rec append lista1 lista2 =
  match lista1 with
  | [] -> lista2
  | x :: resto -> x :: append resto lista2;;


(*val rev_append : 'a list -> 'a list -> 'a list*)
let rec rev_append l1 l2 =
  match l1 with
  | [] -> l2
  | x :: resto -> rev_append resto (x :: l2);;

(*val rev : 'a list list -> 'a list*)
let rev lista =
  let rec aux lista acumulador =
    match lista with
    | [] -> acumulador
    | x :: resto -> aux resto (x :: acumulador)
  in
  aux lista [];;


(*val concat : 'a list list -> 'a list*)
let rec concat listas =
  match listas with
  | [] -> []
  | cabeza :: resto -> append_all cabeza (concat resto)

and append_all lista1 lista2 =
  match lista1 with
  | [] -> lista2
  | x :: resto -> x :: append_all resto lista2;;
  
  
  
(*val flatten : 'a list list -> 'a list*)
let rec flatten lst =
  match lst with
  | [] -> []
  | hd :: tl -> hd @ flatten tl;;

(*val split : ('a * 'b) list -> 'a list * 'b list*)
let rec split pares =
  let rec split_aux pares acumulador1 acumulador2 =
    match pares with
    | [] -> (acumulador1, acumulador2)
    | (x, y) :: resto -> split_aux resto (x :: acumulador1) (y :: acumulador2)
  in
  split_aux pares [] [];;
  

(*val combine : 'a list -> 'b list -> ('a * 'b) list*)
let combine lista1 lista2 =
  let rec combine_aux lista1 lista2 acumulador =
    match (lista1, lista2) with
    | ([], []) -> List.rev acumulador
    | (x :: xs, y :: ys) -> combine_aux xs ys ((x, y) :: acumulador)
    | (_, _) -> failwith "Las listas deben tener la misma longitud."
  in
  combine_aux lista1 lista2 [];;

(*val map : ('a -> 'b) -> 'a list -> 'b list*)
let rec map f lista =
  let rec map_aux f lista acumulador =
    match lista with
    | [] -> acumulador
    | x :: xs -> map_aux f xs ((f x) :: acumulador)
  in
  map_aux f lista [];;


(*val map2 : ('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list*)
let map2 f lista1 lista2 =
  let rec map2_aux f lista1 lista2 acumulador =
    match (lista1, lista2) with
    | ([], []) -> List.rev acumulador
    | (x :: xs, y :: ys) -> map2_aux f xs ys ((f x y) :: acumulador)
    | (_, _) -> invalid_arg "Las listas deben tener la misma longitud."
  in
  map2_aux f lista1 lista2 [];;


(*val rev_map : ('a -> 'b) -> 'a list -> 'b list*)
let rev_map f lista =
  let rec rev_map_aux f lista acumulador =
    match lista with
    | [] -> acumulador
    | x :: xs -> rev_map_aux f xs ((f x) :: acumulador)
  in
  rev_map_aux f lista [];;


(*val for_all : ('a -> bool) -> 'a list -> bool*)
let rec for_all f lista =
  match lista with
  | [] -> true
  | x :: xs -> f x && for_all f xs;;


(*val exists : ('a -> bool) -> 'a list -> bool*)
let rec exists f lista =
  match lista with
  | [] -> false
  | x :: xs -> f x || exists f xs;;

(*val mem : 'a -> 'a list -> bool*)
let rec mem elemento conjunto =
  match conjunto with
  | [] -> false
  | x :: xs -> x = elemento || mem elemento xs;;


(*val find : ('a -> bool) -> 'a list -> 'a*)
let rec find f lista =
  match lista with
  | [] -> raise Not_found
  | x :: xs -> if f x then x else find f xs;;

let es_par x = x mod 2 = 0;;
let lista = [1; 3; 4; 5; 7; 9];;

try
  let resultado = find es_par lista in
  print_int resultado
with
| Not_found -> print_string "No se encontró ningún elemento que cumpla el predicado.\n";;

(*val filter : ('a -> bool) -> 'a list -> 'a list*)
let rec filter f lista =
  match lista with
  | [] -> []
  | x :: xs -> if f x then x :: filter f xs else filter f xs;;


(*val find_all : ('a -> bool) -> 'a list -> 'a list*)
let rec find_all f = function
  | [] -> []
  | x :: xs -> if f x then x :: find_all f xs else find_all f xs;;

(*val partition : ('a -> bool) -> 'a list -> 'a list * 'a list*)
let rec partition f lista =
  match lista with
  | [] -> ([], [])
  | x :: xs ->
      let (cumple, no_cumple) = partition f xs in
      if f x then (x :: cumple, no_cumple)
      else (cumple, x :: no_cumple);;


(*val fold_left : ('a -> 'b -> 'a) -> 'a -> 'b list -> 'a*)
let rec fold_left f acc lista =
  match lista with
  | [] -> acc
  | x :: xs -> fold_left f (f acc x) xs;;

(*val fold_right : ('a -> 'b -> 'b) -> 'a list -> 'b -> 'b*)
let rec fold_right f lista init =
  match lista with
  | [] -> init
  | x :: xs -> f x (fold_right f xs init);;

(*val assoc : 'a -> ('a * 'b) list -> 'b*)
let rec assoc clave lista =
  match lista with
  | [] -> raise Not_found
  | (a, b) :: xs -> if a = clave then b else assoc clave xs;;

(*val mem_assoc : 'a -> ('a * 'b) list -> bool*)
let rec mem_assoc clave lista =
  match lista with
  | [] -> false
  | (a, _) :: xs -> a = clave || mem_assoc clave xs;;


(*val remove_assoc : 'a -> ('a * 'b) list -> ('a * 'b) list*) (* not tail recursive *)
let remove_assoc clave lista =
  let rec aux acc = function
    | [] -> rev_append acc []
    | (a, _) :: xs when a = clave -> rev_append acc xs
    | x :: xs -> aux (x :: acc) xs
  and rev_append acc lst =
    match acc with
    | [] -> lst
    | x :: xs -> rev_append xs (x :: lst)
  in
  aux [] lista;;
