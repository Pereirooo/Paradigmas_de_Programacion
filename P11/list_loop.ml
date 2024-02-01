(*Programación imperactiva: A diferencia de los otros, no vamos a emplear ni definiciones recursivas, pattern-matching y si emplearemos valores tipo alpha ref y bucles*)

(*Prohibido: emplear recursividad, cualquier valor definido fuera del módulo Stlib salvo List.hd y List.tl y tmp se puede usar '@'(concatenar listas)*)


(*let length = List.length;; <-- (*No se puede emplear .length, debemos redefinirla*)*)

let length lst =
  let count = ref 0 in
  let temp = ref lst in
  while !temp <> [] do
    temp := List.tl !temp;
    incr count;
  done;
  !count
;;

let last lst =
  let result = ref (List.hd lst) in
  let temp = ref lst in
  while List.tl !temp <> [] do
    temp := List.tl !temp;
    result := List.hd !temp;
  done;
  !result
;;

let nth lst n =
  let count = ref 0 in
  let result = ref (List.hd lst) in
  let temp = ref lst in
  while !temp <> [] && !count < n do
    temp := List.tl !temp;
    result := List.hd !temp;
    incr count;
  done;
  if !temp = [] then failwith "nth"
  else !result
;;


let rev lst =
  let result = ref [] in
  let temp = ref lst in
  while !temp <> [] do
    result := List.hd !temp :: !result;
    temp := List.tl !temp;
  done;
  !result
;;

let append lst1 lst2 =
  let result = ref lst1 in
  let temp = ref lst2 in
  while !temp <> [] do
    result := List.hd !temp :: !result;
    temp := List.tl !temp;
  done;
  !result
;;

let concat lst =
  let result = ref [] in
  let temp = ref lst in
  while !temp <> [] do
    let current = List.hd !temp in
    temp := List.tl !temp;
    let len = length current in
    let i = ref 0 in
    while !i < len do
      result := (nth current !i) :: !result;
      incr i;
    done;
  done;
  !result
;;


let for_all pred lst =
  let result = ref true in
  let temp = ref lst in
  while !temp <> [] && !result do
    result := pred (List.hd !temp);
    temp := List.tl !temp;
  done;
  !result
;;

let exists pred lst =
  let result = ref false in
  let temp = ref lst in
  while !temp <> [] && not !result do
    result := pred (List.hd !temp);
    temp := List.tl !temp;
  done;
  !result
;;

let find_opt pred lst =
  let result = ref None in
  let temp = ref lst in
  while !temp <> [] && !result = None do
    if pred (List.hd !temp) then
      result := Some (List.hd !temp);
    temp := List.tl !temp;
  done;
  !result
;;

let iter f lst =
  let temp = ref lst in
  while !temp <> [] do
    f (List.hd !temp);
    temp := List.tl !temp;
  done
;;

let fold_left f acc lst =
  let result = ref acc in
  let temp = ref lst in
  while !temp <> [] do
    result := f !result (List.hd !temp);
    temp := List.tl !temp;
  done;
  !result
;;

