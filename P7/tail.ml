(*Redefinir las siguientes funciones tal que no se emplee en ellas recursividad terminal*)
(*Podemos usar funciones recursivas terminales del módulo List como init, fold_left, rev, rev_append, rev_map,etc*)



(*
List.fold_left : ('a -> 'b -> 'a) -> 'a -> 'b list -> 'a List.fold_left f a [b1; ...; bn] es f (... (f (f a b1) b2) ...) bn.

List.rev : 'a list -> 'a list Invierte la lista

List.rev_append : 'a list -> 'a list -> 'a list List.rev_append l1 l2 invierte l1 y lo concatena a l2. Esto es equivalente a List.rev l1 @ l2, pero rev_append emplea recursión de cola y es más eficiente.

List.rev_map : ('a -> 'b) -> 'a list -> 'b list List.rev_map f l da el mismo resultado de List.rev(List.mapf l), pero emplea recursión de cola y es más eficiente.
*)


(*to0from: int -> int list. Devuelve la lista desde n hasta 0*)
(*let rec to0from n =
    if n < 0 then []
    else n :: to0from (n-1);;
*)

let rec to0from n = 
   if n < 0 then []
   else begin
     print_int n;
     print_char ' ';
     to0from (n-1)
   end
;;


(*fromto: int -> int -> int list. Devuelve la lista de números contenidos desde m hasta n*)
(*let rec fromto m n = 
    if m > n then []
    else m :: formto (m+1) n;;
*)

let fromto m n =(*Genera lista de numeros desde m hasta n.*)
  let rec fromto_acc acc start limit = 
    if start > limit then  List.rev acc
    else fromto_acc (start :: acc) ( start + 1) limit
  in 
  fromto_acc [] m n;;


(*remove: 'a -> 'a list -> 'a list. Elimina la primera ocurrencia de un elemento específico de una lista.*)
(*let rec remove x = function
    [] -> []
  | h::t -> if x = h then t
            else h :: remove x t;;
*)

let remove x lst =
  let rec remove_acc acc = function
    | [] -> List.rev acc
    | h :: t ->
      if x = h then
        List.rev_append acc t
      else
        remove_acc (h :: acc) t
  in
  remove_acc [] lst
;;




(*compress: 'a list -> 'a list. Elimina los elementos duplicados consecutivos de una lista.*)
(*let rec compress = function
    h1::h2::t -> if h1 = h2 then compress (h2::t)
                 else h1 :: compress (h2::t)
  | l -> l;;
*)

let compress lst =
  let rec compress_acc acc = function
    | h1 :: h2 :: t when h1 = h2 -> compress_acc acc (h2 :: t)
    | h :: t -> compress_acc (h :: acc) t
    | [] -> List.rev acc
  in
  compress_acc [] lst
;;




(*append': 'a list -> 'a list -> 'a list. Sirve para agregar un elemento al final de la lista*)
(*let append' = List.append;;*)


let append' list1 list2 =
  let rec append_acc acc = function
    | [] -> List.rev_append acc list2
    | h :: t -> append_acc (h :: acc) t
  in
  append_acc [] list1
;;


(*map': ('a -> 'b) -> 'a list -> 'b list. Toma una función y una lista como argumentos y aplica la función a cada elemento de la lista, devolviendo una nueva lista con los resultados.*)
(*let map' = List.map;;*)


let map' f lst =
  let rec map_acc acc = function
    | [] -> List.rev acc
    | h :: t -> map_acc (f h :: acc) t
  in
  map_acc [] lst
;;

(*fold_rigth: ('a -> 'b -> 'b) -> 'a list -> 'b -> 'b. Realiza un plegado(o reduccion) de una lista desde la derecha.*)
(*let fold_right' = List.fold_right;;*)

let fold_right' f lst acc =
  let rec fold_right_acc acc' = function
    | [] -> acc'
    | h :: t -> fold_right_acc (f h acc') t
  in
  fold_right_acc acc (List.rev lst)
;;

(*incseg: int list -> int list. Toma una lista de enteros y produce una nueva lista donde cada elemento es seguido por sus múltiplos incrementados.*)
(*
let incseg l =
    List.fold_right (fun x t -> x::List.map ((+) x) t) l [];;
*)    
    
let incseg l =
  let rec incseg_acc acc = function
    | [] -> List.rev acc
    | x :: t ->
      let multiples = List.map ((+) x) acc in
      incseg_acc (x :: multiples) t
  in
  incseg_acc [] l
;;    
    
