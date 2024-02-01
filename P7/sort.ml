(*Insertar un elemento en una posicion adecuada en una lista ordenada, manteniendo el orden.*)
let rec insert x = function (*Toma como parámetros un valor 'x' y una lista ordenada*)
     [] -> [x](*si la lista está vacía, devuelve una lista que contiene solo 'x'*)
   | h::t -> if x <= h then x :: h :: t(*si tiene al menos un elemento 'h::t', compara 'x' con el 1º elemento 'h' de la lista si es <= se inserta delante*)
             else h :: insert x t;;(*si es mayor, se pasa a llamar recursivamente a insert y seguir comprobando.*)


(*'isort' ordenación por insercción.*)
let rec isort = function (*toma una lista como entrada*)
    [] -> [](*Si la lista está vacía, devuelve una lista vacía.*)
  | h::t -> insert h (isort t);;(*si al menos un elemento, aplica 'insert' para insertar el 1º elemento 'h' en la lista ordenada generada recursivamente por 'isort t'*)
(*ORDENA POR INSERCIÓN: Comienza con una lista vacía y en cada paso inserta el primer elemento no ordenado en la pos. correcta dentro de la parte de la lista que ya está ordenada. Se repite recursivamente hasta que toda la lista esté ordenada.*)




(*Necesitamos una lista lo suficientemente grande como para agotar el espacio disponible en el stack. Con 175000 elementos llega.*)
let rec bigl n acc = (*n=límite superior para construir la lista. (175000), acc=La lista acumuladora que se va construyendo.*)
  if n = 0 then acc (*Si n es igual a 0, la función devuelve la lista acumuladora actual 'acc'(se detiene recursion)*)
  else bigl (n - 1) (n :: acc);;(*Si n no es igual a 0, la función se llama a sí misma de manera recursiva con n - 1*)

let bigl = bigl 175000 [];;(*Al ejecutarlo se crea una lista que contiene los números del 1 al 175.000*)







(*Definir insert_t: 'a -> 'a list -> 'a list y isort_t: 'a list -> 'a list*)
let insert_t x lst = 
  let rec insert_acc acc x = function 
    | [] -> List.rev (x :: acc)
    | h :: t -> 
      if x <= h then 
        insert_acc (x :: acc) h t
      else 
        List.rev_append (h :: acc) (x :: t)
   in 
   insert_acc [] x lst;;


(*Definir isort_t: 'a list -> 'a list que sea recursivo terminal*)
let isort_t lst =
  let rec insert acc x = match acc with
    | [] -> [x]
    | h :: t -> if x <= h then x :: acc else h :: insert t x
  in
  let rec isort acc = function
    | [] -> acc
    | h :: t -> isort (insert acc h) t
  in
  isort [] lst
;;





(*Definir una FALSA FUNCION rlist: int -> int list, tal que para cada n>=0, rlist n devuelva una lista 'aleatoria' con n valores de tipo int. Hágalo de manera que la lista resulte convenientemenete 'dispersa' (que no contenga muchos valores repetidos)*)

(*Genera listas aleatorias dispersas*)
let rlist n =
  let rec generate_random acc remaining =(*funcion auxiliar recursiva que construye la lista*)
    if remaining = 0 then acc
    else
      let random_value = Random.int (2 * n) in(*Random.int (2*n) para generar valores aleatorios en el rango[0.2*n-1]*)
      if List.mem random_value acc then(*Se evita valores duplicados verificando si el valor está ya en la lista mediante List.mem*)
        generate_random acc remaining  (* Evitar valores duplicados *)
      else
        generate_random (random_value :: acc) (remaining - 1)
  in
  generate_random [] n (*Inicia la construccion de la lista*)
;;

(* Configurar la semilla para garantizar resultados reproducibles *)
let _ = Random.init 42;;













(*La función crono nos mide el tiempo de ejecución, necesario para los siguientes apartados*)
let crono f x =
    let t = Sys.time () in
    let _ = f x in
    Sys.time () -. t;;

(*Con la función crono definida, pasamos a medir los tiempos de ejecución entre isort e isort_t con la longitud de la lista. en diferentes situaciones: listas crecientes= lc1, lc2; listas decrecientes= ld1, ld2; y listas aleatorias. LAS 1= 10.000 elementos; LAS 2= 20.000 elementos.*)

(* Definir listas para las pruebas *)
let lc1 = List.init 10000 (fun x -> x);;  
let lc2 = List.init 20000 (fun x -> x);;  
let ld1 = List.init 10000 (fun x -> 9999 - x);;  
let ld2 = List.init 20000 (fun x -> 19999 - x);;  
let lr1 = rlist 10000;;  
let lr2 = rlist 20000;;


(* Comprobación de que isort e isort_t producen el mismo resultado *)

let _ =
  assert (isort lc1 = isort_t lc1);
  assert (isort lc2 = isort_t lc2);
  assert (isort ld1 = isort_t ld1);
  assert (isort ld2 = isort_t ld2)
;;


(* Comprobación del tiempo de ejecución al duplicar el tamaño de la lista *)
let time_isort lst =
  crono isort lst
;;

let time_isort_t lst =
  crono isort_t lst
;;

(* Comprobación con listas crecientes *)
(*En las listas crecientes, empleando isort podemos ver una velocidad de ejecución 3 veces más lenta cuando la lista contiene el doble de elementos(ratio=3.2)*)
let _ =
  let time1 = time_isort lc1 in
  let time2 = time_isort lc2 in
  Printf.printf "Time for isort with lc1: %f seconds\n" time1;
  Printf.printf "Time for isort with lc2: %f seconds\n" time2;
  Printf.printf "Time ratio (lc2/lc1): %f\n" (time2 /. time1)
;;

(* Comprobación con listas decrecientes *)
(*En las listas descendientes vuelve a verse una mayor lentitud en la ejecución en la lista de 20.000 elementos, pero en este caso 5 veces mśa lenta ld2.*)
let _ =
  let time1 = time_isort ld1 in 
  let time2 = time_isort ld2 in 
  Printf.printf "Time for isort with ld1: %f seconds\n" time1;
  Printf.printf "Time for isort with ld2: %f seconds\n" time2;
  Printf.printf "Time ratio (ld2/ld1): %f\n" (time2 /. time1)
;;
  
  
  
(* Comprobación con listas aleatorias dispersas *)
(*Lo interesante es que el tiempo medio en las listas aleatorias al pasarle isort es menor que en las descendientes, aún así la diferencia entre lr1 y lr2 sigue siendo muy parecida a en las descendientes (4.7)*)
let _ =
  let time1 = time_isort_t lr1 in
  let time2 = time_isort_t lr2 in
  Printf.printf "Time for isort_t with lr1: %f seconds\n" time1;
  Printf.printf "Time for isort_t with lr2: %f seconds\n" time2;
  Printf.printf "Time ratio (lr2/lr1): %f\n" (time2 /. time1)
;;

(* Comprobación entre recursiva y recursiva terminal de lista aleatoria dispersas con 20.000 elementos*)
(*Lo interesante de esta prueba es que la función que tiene una recursión terminal es más rápida que la recursión normal, aunque casi inapreciable.*)
let _ =
  let time1 = time_isort lr2 in
  let time2 = time_isort_t lr2 in
  Printf.printf "Time for isort with lr2: %f seconds\n" time1;
  Printf.printf "Time for isort_t with lr2: %f seconds\n" time2;
  Printf.printf "Time ratio (isort lr2/isort_t lr1): %f\n" (time2 /. time1)
;;










(*Función isort_g: ('a -> 'a -> bool) -> 'a list -> 'a list, q proporcione una imple recursiva terminal del algo de ordenación por insercción y que tome como argumento la relación de orden qye se desea emplear para la ordenación.*)
let isort_g ord lst =
  let rec insert_g x acc =
    match acc with
    | [] -> [x]
    | h :: t ->
        if ord x h then x :: acc
        else h :: insert_g x t
  in
  let rec isort_acc acc = function
    | [] -> acc
    | h :: t -> isort_acc (insert_g h acc) t
  in
  isort_acc [] lst
;;







(*Implementación del método de ordenación por mezcla (o fusión)*)

let rec split l = match l with
    h1::h2::t -> let t1, t2 = split t
                 in h1::t1, h2::t2
  | _ -> l, [];;
  
  
let rec merge (l1,l2) = match l1, l2 with
    [], l | l, [] -> l
  | h1::t1, h2::t2 -> if h1 <= h2 then h1 :: merge (t1, l2)
                      else h2 :: merge (l1, t2);;
                      
                      
let rec msort l = match l with
    [] | [_] -> l
  | _ -> let l1, l2 = split l
         in merge (msort l1, msort l2);;
         

(*Si que produce el mismo resultado tanto isort como msort*)



(*split, merge, msort NO están definidas de modo recursivo terminal.Definir bigl2:int list tal que al aplicarle la función msort se produzca un error "stack overflow"*)



(* Definir un valor bigl2 que cause un desbordamiento de pila al aplicar msort *)

let rec bigl2 n acc =
  if n = 0 then acc
  else bigl2 (n - 1) (n :: acc)

let bigl2 = bigl2 175000 [];; 









(*Definir split_t: 'a list -> 'a list * 'a list; versión recursiva terminal de split*)
let split_t l =
  let rec split_acc acc1 acc2 lst =
    match lst with
    | h1 :: h2 :: t -> split_acc (h1 :: acc1) (h2 :: acc2) t
    | _ -> (List.rev acc1, List.rev acc2)
  in
  split_acc [] [] l
;;

(*Definir merge_t: 'a list * 'a list -> 'a list; versión recursiva terminal de merge*)
let merge_t (l1, l2) =
  let rec merge_acc acc l1 l2 =
    match l1, l2 with
    | [], l | l, [] -> List.rev_append acc l
    | h1::t1, h2::t2 ->
      if h1 <= h2 then
        merge_acc (h1 :: acc) t1 l2
      else
        merge_acc (h2 :: acc) l1 t2
  in
  merge_acc [] l1 l2
;;

(*Definir msort': 'a list -> 'a list, empleando split_t y merge_t, en vez de split y merge.*)
let rec msort' l = match l with
    [] | [_] -> l
  | _ -> let l1, l2 = split_t l
         in merge_t (msort' l1, msort' l2);;
   
(*Definir un bigl3: int list que si produzca stackoverflow a msort'*)       
(*Como msort' tiene funciones auxiliares con estructura recursiva terminal no debería causar desbordamiento pero si empleamos una lista muy grande si que podría darse.*)  
let rec bigl3 n acc =
  if n = 0 then acc
  else bigl3 (n - 1) (n :: acc);;

let bigl3 = bigl3 10000 [];;












(*Comprobar los tiempos de ejecución de msort y msort' a los valores lc1, lc2, ld1, ld2, lr1, lr2 y explica resultados*)
let time_msort lst =
  crono msort lst
;;
let time_msort' lst =
  crono msort' lst
;;
(* Comprobación con listas crecientes *)
(*En las listas crecientes, la más pequeña es la que cuenta con un tiempo de ejecución más bajo, algo esperado. La diferencia de velocidad está en torno a 1.7*)
let _ =
  let time1 = time_msort lc1 in
  let time2 = time_msort lc2 in
  Printf.printf "Time for isort with lc1: %f seconds\n" time1;
  Printf.printf "Time for isort with lc2: %f seconds\n" time2;
  Printf.printf "Time ratio (lc2/lc1): %f\n" (time2 /. time1)
;;

(* Comprobación con listas decrecientes *)
(*En el caso de las descendientes, la de 10.000 elementos es la que cuenta con el tiempo de ejecución más rápido. Teniendo una diferencia de aproximadamente el doble.*)
let _ =
  let time1 = time_msort ld1 in 
  let time2 = time_msort ld2 in 
  Printf.printf "Time for isort with ld1: %f seconds\n" time1;
  Printf.printf "Time for isort with ld2: %f seconds\n" time2;
  Printf.printf "Time ratio (ld2/ld1): %f\n" (time2 /. time1)
;;
  
  

(* Comprobación con listas aleatorias dispersas *)
(*Con las aleatorias es muy similar a con las descendentes, más rápido la pequeña y con una diferencia de en torno al doble*)
let _ =
  let time1 = time_msort' lr1 in
  let time2 = time_msort' lr2 in
  Printf.printf "Time for msort_t with lr1: %f seconds\n" time1;
  Printf.printf "Time for msort_t with lr2: %f seconds\n" time2;
  Printf.printf "Time ratio (lr2/lr1): %f\n" (time2 /. time1)
;;

(* Comprobación entre recursiva y recursiva terminal de lista aleatoria dispersas con 10.000 elementos*)
(*Al comparar la diferencia entre la velocidad de msort y msort', se ve como msort'(la recursiva terminal) es más rápida que la versión msort.*)
let _ =
  let time1 = time_msort lr1 in
  let time2 = time_msort' lr1 in
  Printf.printf "Time for msort with lr1: %f seconds\n" time1;
  Printf.printf "Time for msort' with lr1: %f seconds\n" time2;
  Printf.printf "Time ratio (msort lr1/msort' lr1): %f\n" (time2 /. time1)
;;

(* Comprobación entre recursiva y recursiva terminal de lista aleatoria dispersas con 20.000 elementos*)
(*En esta pasa un poco lo mismo qye en la anterior, siendo más rápida msort' y con una diferencia bastante parecida a la del anterior, pero con un tiempo mayor en ambas, como es evidente al emplear una lista más grande.*)
let _ =
  let time1 = time_msort lr2 in
  let time2 = time_msort' lr2 in
  Printf.printf "Time for msort with lr2: %f seconds\n" time1;
  Printf.printf "Time for msort' with lr2: %f seconds\n" time2;
  Printf.printf "Time ratio (msort lr2/msort' lr2): %f\n" (time2 /. time1)
;;

 









(*Implementar msort_g: ('a -> 'a -> bool) -> 'a list -> 'a list; de la ordenación por fusión que tome como argumento la relación de orden a emplear.*)
let msort_g ord lst =
  let rec split_acc acc1 acc2 lst =
    match lst with
    | h1 :: h2 :: t -> split_acc (h1 :: acc1) (h2 :: acc2) t
    | _ -> (List.rev acc1, List.rev acc2)
  in

  let rec merge_acc acc l1 l2 =
    match l1, l2 with
    | [], l | l, [] -> List.rev_append acc l
    | h1 :: t1, h2 :: t2 ->
      if ord h1 h2 then
        merge_acc (h1 :: acc) t1 l2
      else
        merge_acc (h2 :: acc) l1 t2
  in

  let rec msort_acc acc lst =
    match lst with
    | [] | [_] -> List.rev_append acc lst
    | _ ->
      let l1, l2 = split_acc [] [] lst in
      let sorted_l1 = msort_acc [] l1 in
      let sorted_l2 = msort_acc [] l2 in
      merge_acc acc sorted_l1 sorted_l2
  in

  msort_acc [] lst
;;
