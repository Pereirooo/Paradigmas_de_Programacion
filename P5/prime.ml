
(*La siguiente definición no es muy eficiente, nos permite determinar si un número positivo es o no primo*)

let is_prime n =
  let rec check_from i =
    i >= n ||
    (n mod i <> 0 && check_from (i+1))
  in check_from 2;;




(*next_prime*)

let rec next_prime n = 
  let x = n+1 in 
    if is_prime (x) then x
    else next_prime(x);;


(*last_prime*)
let rec last_prime_to n = 
  if is_prime n then  n 
  else last_prime_to (n - 1);;


(*is_prime2*)
let is_prime2 n =
  n >= 2 &&
  let rec sieve primes = function
    | [] -> List.mem n primes
    | p :: rest ->
      p * p > n || (n mod p <> 0 && sieve (p :: primes) rest)
  in
  sieve [] (List.init (int_of_float (sqrt (float_of_int n))) succ);;

    
