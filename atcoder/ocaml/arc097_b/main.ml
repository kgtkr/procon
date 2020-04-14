module UF = struct
  type node = {
    mutable par: int;
    mutable rank: int;
  };;


  let make n = Array.init n (fun i -> { par = i; rank = 0 })
  ;;

  (* 根を求める *)
  let rec find uf x =
    if uf.(x).par == x
    then x
    else
      let par = uf.(x).par in
      let v = find uf par in
      uf.(x).par <- v;
      v
  ;;

  (* xとyの集合を併合 *)
  let unite uf x y =
    let x = find uf x in
    let y = find uf y in
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

  let some uf x y = find uf x == find uf y
  ;;
end

let dec x = x -1
;;

let tuple2map fa fb (a, b) = (fa a, fb b)
;;

let tuple2 a b = (a, b)
;;

let curry2 f a b = f (a, b)
;;

let uncurry2 f (a, b) = f a b
;;

let (>>) f g x = g (f x)
;;

let (>>>) f g x y = g (f x y)
;;

let list_init n f = Array.init n f |> Array.to_list
;;

let () =
  let n, m = Scanf.scanf "%d %d\n" tuple2 in
  let p_list = list_init n (fun _ -> Scanf.scanf "%d " dec) in
  let xy_list = list_init m (fun _ -> Scanf.scanf "%d %d\n" (tuple2 >>> tuple2map dec dec)) in
  let uf = UF.make n in
  List.iter (uncurry2 @@ UF.unite uf) xy_list;
  let res = p_list |> List.mapi tuple2 |> List.filter (uncurry2 @@ UF.some uf) |> List.length in
  Printf.printf "%d\n" res
;;
