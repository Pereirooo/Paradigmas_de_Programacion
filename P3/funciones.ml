(*Definir las siguientes funciones:*)

(*sumto: int -> int, de modo que sumto n, devuelva la suma de los n primeros naturales*)

let rec sumto n = 
  if n < 0 then 
    invalid_arg "n debe ser un número no negativo"
  else
    match n with
    | 0 -> 0
    | _ -> n + sumto (n - 1);;  
    

(*exp10: int -> int, de modo que, para cualquier n >= 0, exp10 n devuelva el valor de 10n*)

let rec exp10 n = 
  if n < 0 then
    invalid_arg "n debe de ser un número no negativo"
  else
    match n with
    | 0 -> 1
    | _ -> 10 * exp10(n - 1);;
    
(*num_cifras: int -> int, de modo que num_cifras n devuelva el número de cifras de la representación decimal de n (el signo no cuenta como cifra)*)


let rec num_cifras n =
  let rec num_cifras_positive n =
    if n < 10 then
      1
    else
      1 + num_cifras_positive (n / 10)
  in
  if n = 0 then
    1
  else
    num_cifras_positive (abs n);;


(*sum_cifras: int -> int, de modo que, sum_cifras n devuelva la suma de las cofras correspondientes a la representación decimal de n.*)

let rec sum_cifras n =
  let rec sum_cifras_positive n =
    if n = 0 then
      0
    else
      (n mod 10) + sum_cifras_positive (n / 10)
  in
  if n = 0 then
    0
  else if n < 0 then
    sum_cifras_positive (-n)
  else
    sum_cifras_positive n;;

