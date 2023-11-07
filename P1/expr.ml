(*Hay que decir de que tipo de error se trata,si es un error de ejecución o decompilación ypor qué*)
();; (*unit*)
2 + 5 * 3;; (*int = 17*)
1.25 *. 2.0;; (*float = 2.5*)
(*2 - 2.0;; Error de ejecución al intentar realizar una operación entre dos tipos distintos de datos*)
(*3.0 + 2.0;; Error de ejecución: operador 'suma entera', frente a dos tipo flotante*)
5 / 3;; (*int = 1, realiza una división entera*)
5 mod 3;; (*int = 2, expresa el 5 en módulo 3*)
2.0 *. 3.0 ** 2.0;; (*float = 18, realiza un producto de flotantes y una potencia*)
2.0 ** 3.0 ** 2.0;; (*float = 512, realiza dos potencias de flotantes*)
sqrt;; (*Nos permite realizar raíces cuadradas*)
(*sqrt 4;; Nos daría error porque sqrt trabaja con punto flotante*)
int_of_float;; (*Nos permite transformar un entero en flotate*)
float_of_int;; (*Combierte un flotante en un entero*)
3.0 = float_of_int 3;; (*Al emplear números en la función, nos permite comprobar si es el número equivalente en el otro tipo, (se convierte en una booleana)*)
(*int_of_float -2.9;; int = -2, nos permite cambiar el tipo de un flotante a un entero*)
int_of_float 2.1 + int_of_float (-2.9);; (*int = -2, nos haría la resta de la parte entera de dichos números*)
truncate;; (*Cambia el tipo pero sin necesidad de estipular, es decir, lee el tipo y lo cambia*)
truncate 2.1 + truncate (-2.9);; (*int = 0, hace la suma de los tipos cambiados*)
floor; (*el mayor entero menor o igual al argumento*)
floor 2.1 +. floor (-2.9);; (*float = -1, es la suma flotante de los mayores menores enteros de los flotantes*)
ceil;; (*el menor entero mayor o igual al argumento*)
ceil 2.1 +. ceil (-2.9);; (*float = 1, hace la suma flotante de los enteros aproximados en base al menor entero mayor al argumento*)
int_of_char;; (*char -> int = <fun>, nos permite hacer una conversión entre caracteres y código ASCII*)
int_of_char 'A';; (*65, corresponde al código de la a mayúscula en la tabla ASCII*)
char_of_int;; (*char -> int = <fun> , nos permite *)
char_of_int 66;; (*char = B, transforma el código ASCII al resultado en char, en este caso la b mayúscula*)
Char.code;; (*Retorna el código ASCII correspondiente a un carácter*)
Char.code 'B';; (*int = 66, otra forma de conversión entre caracteres y su código ASCII*)
Char.chr;; (*Otra forma de retornar el carácter, dando el código ASCII*)
Char.chr 67;; (*char = C, *)
'\067';; (*char = C, otra forma de *)
Char.chr (Char.code 'a' - Char.code 'A' + Char.code 'M');; (*char = 'm', lo que se hace aquí es realizar operaciones con los códigos ASCII de los caracteres y por último devolver el caracter del resultado de las distintas operaciones realizadas*)
Char.lowercase_ascii;; (*Convierte un carácter a minúscula*)
Char.lowercase_ascii 'M';; (*char = 'm'*)
Char.uppercase_ascii;; (*COnvierte un carácter a mayúscula*)
Char.uppercase_ascii 'm';; (*char = 'M'*)
"this is a string";; (*string = "this is a string", al encomillar una cadena de caracteres lo convierte en un string*)
String.length;; (*Retorna la longitud de la cadena*)
String.length "longitud";; (*int = 8, devuelve la longitud de la cadena*)
(*"1999" + "1";; Nos daría un error de ejecución porque no podemos sumar un item a una cadena con el operando de suma de enteros*)
"1999" ^ "1";; (*string = "19991", el símbolo '^', nos permite añadir el 1 a esa cadena*)
int_of_string;; (*Convierte la cadena de dígitios a enteros, sino puede lanza una excepción*)
int_of_string "1999" + 1;; (*int = 2000, al tener el 1 como un entero, hace directamente la suma de enteros entre el 1 y 1999*)
"\065\066";; (*string = "AB", nos permite unificar strings con el hecho de meterlos en unas mismas comillas*)
string_of_int;; (*int -> string = <fun>, nos permite convertir un entero en un string*)
string_of_int 010;; (*string= "10", convierte el entero 10 en un string*)
not true;; (*bool = false, nos devuelve un falso*)
true && false;; (*bool = false, algo no puede ser verdadero y falso al mismo tiempo*)
true || false;; (*bool = true, es verdadero porque algo o es verdadero o es falso pero no puede ser las dos cosas a la vez*)
(1 < 2) = false;; (*bool = false, porque en realidad si que es verdadero que 1 es menor a 2*)
"1" < "2";; (**)
2 < 12;; (*bool = true, porque es verdad que 12 es más grande que 2*)
"2" < "12";; (**)
"uno" < "dos";; (*bool = false, porque aquí en este caso estaríamos comparando tamaños de cadenas y no los números*)
if 3 = 4 then 0 else 4;; (*int = 4, nos devuelve 4 porque 3 = 4 no se cumple*)
if 3 = 4 then "0" else "4";; (*string = "4", por lo mismo que antes*)
(*if 3 = 4 then 0 else "4";; No se puede llevar a cabo ya que mezcla strings con enteros*)
(if 3 < 5 then 8 else 10) + 4;; (*int = 12, porque 8 debido a que 3 < 5 y después 8+4 = 12*)
