(*Esta versión emplea la recursión para explorar todas las posibles configuraciones de reinas en el tablero y acumula las soluciones encontradas en una lista de listas.*)
let rec compatible (i, j) path =
  match path with
  | [] -> true
  | (x, y) :: rest ->
    (* Verifica si la nueva reina amenaza a alguna reina en el camino *)
    i <> x && j <> y && abs (i - x) <> abs (j - y) && compatible (i, j) rest
;;


let queens n =
  let rec completar path i j =
    if i > n then [path]  (* Devuelve la lista de listas con la solución encontrada *)
    else if j > n then []  (* No hay solución para esta fila, devolver lista vacía *)
    else
      let with_queen = if compatible (i, j) path then completar ((i, j) :: path) (i + 1) 1 else [] in
      let without_queen = completar path i (j + 1) in
      with_queen @ without_queen
  in completar [] 1 1;;

	
	
	


(*Función is_queens_sol: int -> (int * int) list -> bool tal que is_queens_sol n sol indique si la lista sol es una solución válida para el problema de las n reinas.*)
let is_queens_sol n sol =
  let rec compatible_pos (i, j) rest =
    match rest with
    | [] -> true
    | (x, y) :: tl ->
      i <> x && j <> y && abs (i - x) <> abs (j - y) && compatible_pos (i, j) tl
  in

  let rec no_conflict pos_list =
    match pos_list with
    | [] -> true
    | pos :: tl -> compatible_pos pos tl && no_conflict tl
  in

  List.length sol = n && no_conflict sol
;;

let check n = List.for_all (is_queens_sol n) (queens n);;















(*
(*queens: int -> (int * int) list list   [queens n]*)
let rec compatible (i, j) path =
  match path with
  | [] -> true
  | (x, y) :: rest ->
    (* Verifica si la nueva reina amenaza a alguna reina en el camino *)
    i <> x && j <> y && abs (i - x) <> abs (j - y) && compatible (i, j) rest
  in completa [] 1, 1;;  
;;


(*Este es el código proporcionado por el porfesor en clase de teoría*)

let queens n = (*Función que toma un numero 'n' como parámetro y devuelve la sol. al problema de las damas*)
  let rec completar path i j = (*completar es función auxiliar que se encarga de completar una fila del tablero con una reina en la pos(i,j). Arguments: 'path'=lista con reinas ya colocadas, 'i'=la fila actual, 'j'=la columna actual*)
     if i>n then path(*i>n <-- verifica si hemos alcanzado la última fila, then: devolver 'some path',por lo que hemos encontrado una sol. válida y procedemos a devolver la lista de pos. de reinas*)
     else if j>n then raise Not_found(*j>n <-- verifica si llegamo al final de la fila actual,then: lanza 'Not Found', indicando solución no encontrada en esa fila y debemos retroceder.*)
     else if compatible (i, j) path(*verifica si la pos (i,j) es compatible con las reinas YA colocadas. fun 'compatible' no está definida pero asumimos que verifica si la new reina amenaza alguna reina ya colocada*)
             then try completar ((i, j):: path) (i+1) 1 with(*si pos.(i,j) es compatible, intenta llamar recursivamente a 'completar' con la nueva pos. de la reina añadida a la list 'path' *)
                    Not_found -> completar path i (j+1)(*Si no puede colocar una reina en la columna actual ('Not found'), intenta en la siguiente columna de la misma fila ('completar path i (j+1))*)
             else completar path i(j+1)(*Si la pos. no es compatible, intenta con la siguiente columna de la misma fila ('completar path i (j+1))*)
      in completa [] 1 1;;(*Finalmente 'queens' se llama inicialmente con una lista vacía [], la primera fila ('1'), y la primera columna ('1'). Esto inicia el proceso de búsqueda de soluciones.*)

(*Ahora debemos de hacer nuestro queens tal y como dice en el enunciado*)


(*la funcion debe de ser de tipo queens: int -> (int * int) list list*)





(*is_queens_sol: int -> (int * int) list -> bool, indica si n sol es una solucion válida para el problema de las n reinas. No se debe de utilizar la función queens en la solución*)
	
let is_queens_sol solution n =
  let rec is_compatible (x, y) = function
    | [] -> true
    | (a, b)::tl ->
      x <> a && y <> b && abs (x - a) <> abs (y - b) && is_compatible (x, y) tl
  in
  match solution with
  | None -> false  (* No se encontró ninguna solución *)
  | Some queens ->
    if List.length queens <> n then false  (* La cantidad de reinas no es la esperada *)
    else
      List.for_all (fun queen -> is_compatible queen queens) queens;;


*)
