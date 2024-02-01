let g n = (n >= 0 && n mod 2 = 0) || n mod 2 = -1;;

(*Hay que describir dicha función pero omitiendo los ands y los ors, empleando sentencias if-then-else*)

let g1 n =
 if (n >= 0) then 
    n mod 2 = 0
 else
   n mod 2 = -1;;
   
(*Ahora debemos de tener en cuenta las condiciones anteriores y además no debe apareer ninguna sentencia if-then-else*)

let g2 n = (function true -> true | false -> n mod 2 = -1)
      ((function true -> n mod 2 = 0 | false -> false) (n >= 0));;
