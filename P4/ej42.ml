(*Redefinir las funciones (min, max, fst y snd) sin utilizar dichas funciones pertenecientes al módulo Stdlib*)

(*min*)
let min x y =
  if x < y then x else y;;

(* Ejemplo de uso *)
let resul = min 10 5;;
print_int resul;;


(*max*)
let max x y =
  if x > y then x else y;;
  
let resul = max 10 5;;
print_int resul;;


(*fst*)
let fst (x, _) = x;;

(*Ejemplo uso*)

let elemento1 = fst (3, 7);;
print_int elemento1;;
    


(*snd*)
let snd (_, y) = y;;

(*Ejemplo de uso*)
let elemento2 = snd (4, 8);;
print_int elemento2;;


