(* The package sedlex is released under the terms of an MIT-like license. *)
(* See the attached LICENSE file.                                         *)
(* Copyright 2005, 2013 by Alain Frisch and LexiFi.                       *)

type t =
  | Chars of Cset.t
  | Seq of t list
  | Alt of t list
  | Star of t
  | Plus of t
  | Rep of t * int * int
  | Eps
  | Capture of string * t

module SSet = Set.Make (String)

let rec capture_names_acc acc = function
  | Chars _ | Eps -> acc
  | Capture (name, inner) -> capture_names_acc (SSet.add name acc) inner
  | Seq elems -> List.fold_left capture_names_acc acc elems
  | Alt branches -> List.fold_left capture_names_acc acc branches
  | Star inner | Plus inner | Rep (inner, _, _) -> capture_names_acc acc inner

let capture_names t = capture_names_acc SSet.empty t

let reject_captures ctx t =
  if SSet.is_empty (capture_names t) then Ok t
  else Error (Printf.sprintf "'as' bindings are not supported inside %s" ctx)

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
  | Alt branches -> (
      match List.map fixed_length branches with
        | [] -> None
        | first :: rest ->
            if List.for_all (( = ) first) rest then first else None)
  | Rep (inner, n, m) ->
      if n = m then (
        match fixed_length inner with Some l -> Some (n * l) | None -> None)
      else None
  | Star _ | Plus _ -> None

(* Smart constructors *)

let chars c = Chars c
let eps = Eps

let capture name inner =
  if SSet.mem name (capture_names inner) then
    Error
      (Printf.sprintf
         "'as' binding '%s' shadows an inner binding of the same name" name)
  else Ok (Capture (name, inner))

let star t =
  match reject_captures "Star" t with Error _ as e -> e | Ok t -> Ok (Star t)

let plus t =
  match reject_captures "Plus" t with Error _ as e -> e | Ok t -> Ok (Plus t)

let rep t n m =
  match reject_captures "Rep" t with
    | Error _ as e -> e
    | Ok t -> Ok (Rep (t, n, m))

let seq a b =
  match (a, b) with
    | Eps, x | x, Eps -> x
    | Seq l1, Seq l2 -> Seq (l1 @ l2)
    | Seq l1, x -> Seq (l1 @ [x])
    | x, Seq l2 -> Seq (x :: l2)
    | _ -> Seq [a; b]

let alt a b =
  let branches =
    match (a, b) with
      | Chars c1, Chars c2 -> [Chars (Cset.union c1 c2)]
      | Alt l1, Alt l2 -> l1 @ l2
      | Alt l1, x -> l1 @ [x]
      | x, Alt l2 -> x :: l2
      | _ -> [a; b]
  in
  let names = List.map capture_names branches in
  match names with
    | [] | [_] -> Ok (match branches with [x] -> x | _ -> Alt branches)
    | first :: rest ->
        if List.for_all (SSet.equal first) rest then
          Ok (match branches with [x] -> x | _ -> Alt branches)
        else Error "all branches of '|' must bind the same names with 'as'"

(* All structural constraints are enforced by the smart constructors.
   [check_invariant] verifies them as a debug assertion. *)
let check_invariant t =
  let rec check ~inside_rep = function
    | Chars _ | Eps -> ()
    | Capture (name, inner) ->
        assert (not inside_rep);
        assert (not (SSet.mem name (capture_names inner)));
        check ~inside_rep inner
    | Seq elems ->
        assert (List.length elems >= 2);
        List.iter (check ~inside_rep) elems
    | Alt branches ->
        assert (List.length branches >= 2);
        (match List.map capture_names branches with
          | first :: rest -> assert (List.for_all (SSet.equal first) rest)
          | [] -> assert false);
        List.iter (check ~inside_rep) branches
    | Star inner | Plus inner -> check ~inside_rep:true inner
    | Rep (inner, _, _) -> check ~inside_rep:true inner
  in
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
  | Alt branches ->
      Format.fprintf fmt "(%a)"
        (Format.pp_print_list
           ~pp_sep:(fun fmt () -> Format.fprintf fmt " | ")
           pp)
        branches
  | Star inner -> Format.fprintf fmt "Star %a" pp inner
  | Plus inner -> Format.fprintf fmt "Plus %a" pp inner
  | Rep (inner, n, m) -> Format.fprintf fmt "Rep(%a, %d..%d)" pp inner n m
  | Capture (name, inner) -> Format.fprintf fmt "(%a as %s)" pp inner name

let show t = Format.asprintf "%a" pp t
