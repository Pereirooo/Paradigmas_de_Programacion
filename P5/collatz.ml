let f n = if n mod 2 = 0 then n / 2 else 3 * n + 1 ;;


(*ORBITA*)
let rec orbit n =
  if n < 0 then
    raise (Invalid_argument "El número debe ser un entero mayor a 0.")
  else if n = 1 then
    "1"
  else
    string_of_int n ^","^ orbit(f n) ;;

(*Length*)

let rec length n =
    if n <= 0 then
      raise (Invalid_argument "El número debe ser positivo")
    else if n = 1 then
      0
    else
      let f_resultado = f n in
      1 + length(f_resultado);;	 
  
(*top n*)
let rec top n = 
    if n < 0 then 
      raise (Invalid_argument "El numero debe ser un entero mayor a 0.")
    else if n = 1 then 
    1
    else let next = f n in 
    max n (top next);;
  
   
   
 
(*length'n'to*)
let length'n'top n =
  let rec aux num_pasos max_height current_n =
    if current_n <= 1 then (num_pasos + 1, max max_height 1)
    else
      let next_height = f current_n in
      aux (num_pasos + 1) (max max_height next_height) next_height
  in
  if n < 0 then
    raise (Invalid_argument "El número debe ser positivo")
  else
    aux 0 0 n;;




(*longest_in*)
let rec longest_in n m =
	if n=m 
	then n,length n
	else if (m <= 0 || m < 0) then 
	raise(Invalid_argument "Los números deben ser mayores a 0.")
	else let x,y = longest_in (n + 1) m in
		let length_n = length n in
			if length_n >= y then n,length_n
			else x,y;;
    

(*highest_in*)

let highest_in m n =
  let rec orbit_height max_height current_n =
    if current_n <= 1 then max max_height 1
    else orbit_height (max max_height current_n) (f current_n)
  in
  let rec find_highest_height min_num max_height current_num =
    if current_num > n then (min_num, max_height)
    else
      let current_height = orbit_height 0 current_num in
      if current_height > max_height then
        find_highest_height current_num current_height (current_num + 1)
      else
        find_highest_height min_num max_height (current_num + 1)
  in
  if m <= 0 || n <= 0 then
    raise (Invalid_argument "Los números deben ser positivos.")
  else
    find_highest_height m 0 m;;
