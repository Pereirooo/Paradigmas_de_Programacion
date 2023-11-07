(*Tenemos que definir dentro de archivo los siguientes valores:*)


(*Un valor v de tipo int a partir de una expresión que contenga al menos una funcion predefinida*)


let v = succ 2;;




(*Un valor w de tipo float a partir de una expresión que contenga al menos 4 operadores infijos*)

let w = 2.0 +. 3.5 *. 1.3 -. 5.3 /. 2.1;;




(*Un valor x de tipo char a partir de una expresión que contenga una frase if-then-else*)
(*
let x =
  if x = 'a' || x = 'e' || x = 'i' || x = 'o' || x = 'u' then "Es una vocal"
  else
  "No es una vocal";;
*)

let condicion = true;;

let x =
  if condicion then
    'A'
  else
    'B';;

print_char x;;

  
(*Un valor y de tipo bool a partir de una expresion que contenga una o mas funciones u operadores*)

let y=3;;
let y =
    if y > 2 then true 
    else false;;



(*Un valor z de tipo string a partir de una expresion que contenga una sub-expresion de tipo int*)

let num = 15;;
let z = "El valor de la variable num es: " ^ string_of_int num;;

