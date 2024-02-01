let append arr1 arr2 =
  let len1 = Array.length arr1 in
  let len2 = Array.length arr2 in
  let result = Array.init (len1 + len2) (fun i ->
    if i < len1 then arr1.(i) else arr2.(i - len1)
  ) in
  result
;;

let sub arr start len =
  let result = Array.init len (fun i -> arr.(start + i)) in
  result
;;

let copy arr =
  let len = Array.length arr in
  let result = Array.init len (fun i -> arr.(i)) in
  result
;;

let fill arr start len value =
  for i = start to start + len - 1 do
    arr.(i) <- value
  done
;;

let blit src src_pos dst dst_pos len =
  for i = 0 to len - 1 do
    dst.(dst_pos + i) <- src.(src_pos + i)
  done
;;

let to_list arr =
  let len = Array.length arr in
  let result = ref [] in
  for i = len - 1 downto 0 do
    result := arr.(i) :: !result
  done;
  !result
;;

let of_list lst =
  let len = List.length lst in
  let result = Array.init len (List.nth lst) in
  result
;;

let iter f arr =
  let len = Array.length arr in
  for i = 0 to len - 1 do
    f arr.(i)
  done
;;

let fold_left f acc arr =
  let len = Array.length arr in
  let result = ref acc in
  for i = 0 to len - 1 do
    result := f !result arr.(i)
  done;
  !result
;;

let for_all pred arr =
  let len = Array.length arr in
  let result = ref true in
  for i = 0 to len - 1 do
    result := pred arr.(i);
    if not !result then
      raise Exit
  done;
  !result
;;

let exists pred arr =
  let len = Array.length arr in
  let result = ref false in
  try
    for i = 0 to len - 1 do
      result := pred arr.(i);
      if !result then
        raise Exit
    done;
    !result
  with Exit -> !result
;;

let find_opt pred arr =
  let len = Array.length arr in
  let result = ref None in
  try
    for i = 0 to len - 1 do
      if pred arr.(i) then begin
        result := Some arr.(i);
        raise Exit
      end
    done;
    !result
  with Exit -> !result
;;

