(* The package sedlex is released under the terms of an MIT-like license. *)
(* See the attached LICENSE file.                                         *)
(* Copyright 2005, 2013 by Alain Frisch and LexiFi.                       *)

type t =
  | Chars of Cset.t
  | Seq of t list
  | Alt of t * t
  | Star of t
  | Plus of t
  | Rep of t * int * int
  | Eps
  | Capture of string * t

(* Smart constructors *)

let chars c = Chars c
let eps = Eps
let capture name inner = Capture (name, inner)
let star t = Star t
let plus t = Plus t
let rep t n m = Rep (t, n, m)

let seq a b =
  match (a, b) with
    | Eps, x | x, Eps -> x
    | Seq l1, Seq l2 -> Seq (l1 @ l2)
    | Seq l1, x -> Seq (l1 @ [x])
    | x, Seq l2 -> Seq (x :: l2)
    | _ -> Seq [a; b]

let alt a b =
  match (a, b) with
    | Chars c1, Chars c2 -> Chars (Cset.union c1 c2)
    | _ -> Alt (a, b)

(* Analysis *)

let rec fixed_length = function
  | Chars _ -> Some 1
  | Eps -> Some 0
  | Capture (_, inner) -> fixed_length inner
  | Seq elems ->
      List.fold_left
        (fun acc e ->
          match (acc, fixed_length e) with
            | Some a, Some b -> Some (a + b)
            | _ -> None)
        (Some 0) elems
  | Alt (a, b) -> (
      match (fixed_length a, fixed_length b) with
        | Some n1, Some n2 when n1 = n2 -> Some n1
        | _ -> None)
  | Rep (inner, n, m) ->
      if n = m then (
        match fixed_length inner with Some l -> Some (n * l) | None -> None)
      else None
  | Star _ | Plus _ -> None

let rec capture_names_acc acc = function
  | Chars _ | Eps -> acc
  | Capture (name, inner) ->
      let acc = if List.mem name acc then acc else name :: acc in
      capture_names_acc acc inner
  | Seq elems -> List.fold_left capture_names_acc acc elems
  | Alt (a, b) -> capture_names_acc (capture_names_acc acc a) b
  | Star inner | Plus inner | Rep (inner, _, _) -> capture_names_acc acc inner

let capture_names t = capture_names_acc [] t |> List.sort_uniq String.compare

(* Validation *)

let validate t =
  let rec check ~inside_rep t =
    match t with
      | Chars _ | Eps -> Ok ()
      | Capture (_, _) when inside_rep ->
          Error "'as' bindings are not supported inside repetition operators"
      | Capture (name, inner) ->
          if List.mem name (capture_names inner) then
            Error
              (Printf.sprintf
                 "'as' binding '%s' shadows an inner binding of the same name"
                 name)
          else check ~inside_rep inner
      | Seq elems ->
          List.fold_left
            (fun acc e ->
              match acc with Error _ -> acc | Ok () -> check ~inside_rep e)
            (Ok ()) elems
      | Alt (a, b) ->
          let* () = check ~inside_rep a in
          let* () = check ~inside_rep b in
          let na = capture_names a in
          let nb = capture_names b in
          if na <> nb && (na <> [] || nb <> []) then
            Error "both sides of '|' must bind the same names with 'as'"
          else Ok ()
      | Star inner | Plus inner -> check ~inside_rep:true inner
      | Rep (inner, _, _) -> check ~inside_rep:true inner
  and ( let* ) r f = match r with Error _ as e -> e | Ok x -> f x in
  check ~inside_rep:false t

(* Pretty-printing *)

let rec pp fmt = function
  | Chars cset -> (
      let intervals = (cset : Cset.t :> (int * int) list) in
      match intervals with
        | [(c, c')] when c = c' ->
            if c >= 32 && c <= 126 then Format.fprintf fmt "'%c'" (Char.chr c)
            else Format.fprintf fmt "0x%04X" c
        | _ ->
            Format.fprintf fmt "[%a]"
              (Format.pp_print_list
                 ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ")
                 (fun fmt (lo, hi) ->
                   if lo = hi then
                     if lo >= 32 && lo <= 126 then
                       Format.fprintf fmt "'%c'" (Char.chr lo)
                     else Format.fprintf fmt "0x%04X" lo
                   else Format.fprintf fmt "0x%04X-0x%04X" lo hi))
              intervals)
  | Eps -> Format.fprintf fmt "eps"
  | Seq elems ->
      Format.fprintf fmt "(%a)"
        (Format.pp_print_list
           ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ")
           pp)
        elems
  | Alt (a, b) -> Format.fprintf fmt "(%a | %a)" pp a pp b
  | Star inner -> Format.fprintf fmt "Star %a" pp inner
  | Plus inner -> Format.fprintf fmt "Plus %a" pp inner
  | Rep (inner, n, m) -> Format.fprintf fmt "Rep(%a, %d..%d)" pp inner n m
  | Capture (name, inner) -> Format.fprintf fmt "(%a as %s)" pp inner name

let show t = Format.asprintf "%a" pp t
