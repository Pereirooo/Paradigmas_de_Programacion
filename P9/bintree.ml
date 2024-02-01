(*debemos definir el tipo de dato llamado 'binary_tree' que representa un árbol binario. Puede ser Empty(vacío) ó un nodo 'Node' que contiene un `'a` y dos subarboles tipo 'a binary_tree' que representan el subárbol izquierdo y el derecho respectivamente.*)
type 'a bintree = 
  | Empty
  | Node of 'a * 'a bintree * 'a bintree;;

(*in_orden es la función que realiza el recorrido in_order, es recursiva y emplea el patern-matching para manejar los 2 casos: 'Empty' y 'Node'*)
let rec in_order tree =
  match tree with
  | Empty -> [](*Si es empty, entonces devuelve una lista vacía*)
  | Node (value, left, right) ->
    (in_order left) @ [value] @ (in_order right);;
(*Si hay un nodo con un valor y dos subarboles, se realiza el recorrido inorden del subarbol izquierdo. (in_order_traversal left), concatena el valor del nodo ([value]) y luego realiza el recorrido inorden del subárbol derecho (in_order_traversal right). Finalmente empleamos @ para concatenar listas.*)






(*Una función insert de modo que insert ord tree x sea un árbol binario de búsqueda conteniendo los nodos que tenía tree más un único nodo adicional con valor x.*)
let rec insert ord tree x =(*ord=funcion de comparacion, tree=arbol binario de busqueda, 'x'=valor a insertar en el arbol.*)
  match tree with(*Maneja los dos casos posibles*)
  | Empty -> Node (x, Empty, Empty)(*Si el árbol está vacío se crea el nodo con el valor x sin sub tree*)
  | Node (value, left, right) ->(*Teniendo un nodo conun valor,se emplea ord para comparar x con value *)
    if ord x value then(*True, then x menor o igual que value, entonces se inserta x en el subtr izq.*)
      Node (value, insert ord left x, right)
    else(*si 'ord x value es falso, x es mayor que value, entonces se inserta x en subtr rigt llamando recursivamente a insert*)
      Node (value, left, insert ord right x);;
(*En ambos casos mantenemos el valor original del nodo actual 'value' y se actualizan los subarboles derecho e izquierdo según el resultado de la comparación.*)







(*Ahora definimos (EMPLEANDO INSERT) una función bst: ('a -> 'a -> bool) -> 'a list -> 'a bintree, de modo que, si ord es una relacion de orden, bst ord l sea un arbol binario de búsqueda (según la rela de orden ord) que contenga un nodo para cada valor de la lista I.*)
let rec bst ord lst =
  List.fold_left (fun tree x -> insert ord tree x) Empty lst;;






(*Empleando bst e in_order construir fun qsort: ('a -> 'a -> bool) -> 'a list -> 'a list, que ordene cualquier lista segu-ún el orden indicado. Se basa en el algoritmo de ordenación 'quick sort'.*)
let rec qsort ord lst = (*qsort es recursiva y toma como parámetros 'ord'=relación de orden y 'lst'=una lista*)
  let tree = bst ord lst in(*emplea bst construir árbol binario de búsqueda a partir de la lista, para luego -->*)
  in_order tree;;(*aplicar la función in_order al propio árbol*)





