let tuple3 a b c = (a, b, c)
;;

let tuple4 a b c d = (a, b, c, d)
;;

let rec f n m list a_list min =
  if Array.length a_list == n
  then
    list
    |> List.filter_map
      (fun (a, b, c, d) -> if a_list.(b-1) - a_list.(a-1) == c then Option.some d else Option.none) 
    |> List.fold_left (+) 0
  else
    List.init (m - min + 1) ((+) min)
    |> List.map (fun min -> f n m list (Array.append a_list [|min|]) min)
    |> List.fold_left max 0
;;

let () =
  let n, m, q = Scanf.scanf "%d %d %d\n" tuple3 in
  let list = List.init q (fun _ -> Scanf.scanf "%d %d %d %d\n" tuple4) in
  let res = f n m list [||] 1 in
  Printf.printf "%d\n" res
;;