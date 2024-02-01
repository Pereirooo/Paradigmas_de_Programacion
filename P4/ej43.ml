(*Función reverse que invierta el orden de las cifras de la representación decimal de n.*)

let rec reverse n = 
  if n<10 then n
  else
    let last_digit = n mod 10 in
    let rest_of_digits = n/10 in
    let reversed_rest = reverse rest_of_digits in
    let reversed_rest_length = int_of_float (log10(float reversed_rest))+1 in
    (last_digit * int_of_float (10. ** float reversed_rest_length)) + reversed_rest;;  
  
  
 
(*Crear una función que compruebe si una función se trata de un palíndromo o no*)

let rec palindromo s =
  let len = String.length s in
  if len <= 1 then
    true
  else
    s.[0] = s.[len - 1] && palindromo (String.sub s 1 (len - 2));;


let resultado = palindromo "reconocer";;



(*Crear función recursiva llamada mcd que calcule el máximo como un divisor de dos números*)

let rec mcd : int * int -> int = fun (x, y) ->
  if y = 0 then
    x
  else if x = 0 then
    y
  else if x > y then
    mcd (y, x mod y)
  else
    mcd (x, y mod x);;
