type uf_node = {
  mutable par: int;
  mutable rank: int;
}
;;

let uf_make n = Array.init n (fun i -> { par = i; rank = 0 })
;;

(* 根を求める *)
let rec uf_find uf x =
  if uf.(x).par == x
  then x
  else
    let par = uf.(x).par in
    let v = uf_find uf par in
    uf.(x).par <- v;
    v
;;

(* xとyの集合を併合 *)
let uf_unite uf x y =
  let x = uf_find uf x in
  let y = uf_find uf y in
  if x != y then
    if uf.(x).rank < uf.(y).rank
    then (
      uf.(x).par <- y;
      ()
    )
    else (
      uf.(y).par <- x;
      if uf.(x).rank == uf.(y).rank then (
        uf.(x).rank <- uf.(x).rank + 1;
        ()
      )
    )
;;

let uf_some uf x y = uf_find uf x == uf_find uf y
;;

let rec parse_xy_list_ m l =
  if m == 0
  then l
  else parse_xy_list_ (m - 1) ((match (read_line () |> Str.split (Str.regexp " ") |> List.map int_of_string : int list) with | [x;y ] -> x, y | _ -> raise Not_found) :: l)
;;

let parse_xy_list m = parse_xy_list_ m [] |> List.rev
;;

let () =
  let n, m = match read_line () |> Str.split (Str.regexp " ") |> List.map int_of_string with
    | [n; m] -> n, m
    |  _ -> raise Not_found in
  let p_list = read_line () |> Str.split (Str.regexp " ") |> List.map int_of_string in
  let xy_list = parse_xy_list m in
  let uf = uf_make n in
  List.iter (fun xy -> let x, y = xy in uf_unite uf (x - 1) (y - 1)) xy_list;
  let res = p_list |> List.mapi (fun i x -> uf_some uf i (x - 1)) |> List.filter (fun x -> x) |> List.length in
  Printf.printf "%d\n" res
;;
