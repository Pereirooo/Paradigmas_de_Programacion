(*x - y;;En principio tendremos un error debido a que falta declarar las variables*)
let x = 1;;(*val x: int = 1*)
(*x - y;;Falta declarar la variable y*)
let y = 2;;(*val y: int = 2, inicializa la variable y dándole el valor de 2*)
x - y;;(*int = -1, nos devuelve el resultado de la resta que en este caso es -1*)
(*let x = y in x - y;;int = 0, realiza la operación de igual x a y cuando x-y*)
(*La nueva definición seria:*)
(function x -> x - y) y;;
x - y;;
(*z;;Nos da un error porque la variable no está inicializada*)
let z = x + y;;(*int = 3, estaríamos guardando el valor resultante de la suma de las dos variables anteriores, es decir, 1+2 --> z*)
z;;(*int = 3, nos estaría devolviendo el valor guardado en dicha variable*)
let x = 5;;(*int = 5, estaríamos guardando el valor 5 en la variable 5*)
x + y;;(*int = 7, este x + y nos cambia el resultado debido al cambio del valor guardado en la variable x*)
z;;(*int = 3, nos devolvería el mismo valor ya que el valor guardado dentro de la función no a cambiado*)
(*let y = 5 in x+y;;int = 10, vuelve a ser otro entero ya que cambiamos otra vez el valor alojado en la variable y*)
(function y -> x + y) 5;;
x + y;;(**)
(*let x = x + y in let y = x * y in x + y +z;;*)
(function x -> (function y -> x + y + z) (x * y)) (x + y);;
x + y + z;;(*int = 13*)
function x -> 2 * x;;(*int = 10, función que devuelve el doble del valor de la variable x*)
(function x -> 2 * x) (2 + 1);;(*int = 6, aplica la función con x = 3, por los paréntesis*)
(function x -> 2 * x) 2 + 1;;(*int = 5, aplica la función con x = 2, al no tener paréntesis, y le suma 1*)
let f = function x -> 2 * x;;(*permite guardar el valor de la función en la variable f*)
f;;(**)
f (2 + 1);;(*Aplicaríamos la función de arriba para el valor dentro del paréntesis, es decir x = 3*)
f 2 + 1;;(*Parecido a la anterior vez, aplicaría la función con un x = 2 y al result le suma 1*)
f x;;(**)
let x = 100;;(*val x: int = 100, almacenaría el entero 100 dentro de la variable x*)
f x;;(*aplicaría la función almacenada en f, contando con un x = 100*)
let m = 1000;;(*val m: int = 1000, guarda el entero 1000, almacenado en la variable 1000*)
let g = function x -> x + m;;(*val g : int -> int = <fun>, guarda en f el valor de la suma de x + m*)
g;;
g 3;;(*int = 1003, añade/suma 3 a m*)
(*g 3.0;;Sería un error de tipo, estaríamos metiendo un tipo float en una función que espera un tipo entero*)
let m = 7;;(*val m: int = 7, guarda el valor, entero 7, dentro de la variable m*)
g 3;;
let istrue = function true -> true;;(*En este caso, nos da un error de pattern-matching, que viene dado porque no están definidas todas las posibilidades, es decir, en este caso tenemos una funcion de true a true, pero no contempla lo que pasaría si en vez de empezar en true empezase en false, ya que si empieza en true sabemos que devuelve true, pero si empieza en false que devuelve? de ahí el pattern-matching*)
let istrue = function false -> false;;
(*let istrue = function false -> false;;*)
istrue;;(*true -> true*)
istrue (1 < 2);;(*bool = true, debería devolver true, ya que si se cumple que 1 < 2*)
istrue (2 < 1);;(*bool = false, ya que es una secuencia falsa*)
(*istrue 0;;error de compilación, estamos introduciendo un int, cuando se espera una secuencia a comprobar*)
let iscero_v1 = function 0 -> true;;(*int -> bool = <fun>*)
iscero_v1 0;;(*bool = true*)
(*iscero_v1 0.;;nos da un error de ejecución debido a q estamos empleando un tipo float cuando debemos utilizar un tipo int*)
iscero_v1 1;;

let iscero_v2 = function 0 -> true | _ -> false;;
iscero_v2 0;;(*bool = true*)
iscero_v2 1;;(*bool = false*)
(*iscero_v2 0.;;error, solo admite tipos int, y este se trata de un float*)
let all_to_true = function true -> true | false -> true;;(*val all_to_true: bool -> bool = <fun>*)
all_to_true (1 < 2);;(*bool = true*)
all_to_true (2 < 1);;(*bool = true*)
(*all_to_true 0;;es de tipo int y debe ser de tipo bool*)
let first_all_to_true = all_to_true;;(*val first_all_to_true : bool -> bool = <fun>*)
let all_to_true = function x -> true;;(*val all_to_true : 'a -> bool = <fun>*)
all_to_true (1 < 2);;(*bool = true*)
all_to_true (2 < 1);;(*bool = true*)
all_to_true 0;;(*bool = true*)
(*first_all_to_true 0;;Error de ejecución: es un int y se esperaba un bool*)
