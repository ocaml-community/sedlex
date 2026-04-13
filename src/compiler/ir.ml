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
  if not (0 <= n && n <= m) then Error "Invalid range for Rep operator"
  else (
    match reject_captures "Rep" t with
      | Error _ as e -> e
      | Ok t -> Ok (Rep (t, n, m)))

let seq a b =
  match (a, b) with
    | Eps, x | x, Eps -> x
    | Seq l1, Seq l2 -> Seq (l1 @ l2)
    | Seq l1, x -> Seq (l1 @ [x])
    | x, Seq l2 -> Seq (x :: l2)
    | _ -> Seq [a; b]

let alt a b =
  let an = capture_names a in
  let bn = capture_names b in
  if not (SSet.equal an bn) then
    Error "all branches of '|' must bind the same names with 'as'"
  else
    Ok
      (match (a, b) with
        | Chars c1, Chars c2 -> Chars (Cset.union c1 c2)
        | Alt l1, Alt l2 -> Alt (l1 @ l2)
        | Alt l1, x -> Alt (l1 @ [x])
        | x, Alt l2 -> Alt (x :: l2)
        | _ -> Alt [a; b])

(* All structural constraints are enforced by the smart constructors.
   [check_invariant] verifies them as a debug assertion. *)
let check_invariant t =
  let rec check = function
    | Chars _ | Eps -> SSet.empty
    | Capture (name, inner) ->
        let names = check inner in
        assert (not (SSet.mem name names));
        SSet.add name names
    | Seq elems ->
        assert (List.length elems >= 2);
        List.fold_left (fun acc x -> SSet.union acc (check x)) SSet.empty elems
    | Alt [] | Alt [_] -> assert false
    | Alt (first :: rest) ->
        let names = check first in
        List.iter (fun x -> assert (SSet.equal names (check x))) rest;
        names
    | Star inner | Plus inner ->
        assert (SSet.is_empty (check inner));
        SSet.empty
    | Rep (inner, _, _) ->
        assert (SSet.is_empty (check inner));
        SSet.empty
  in
  let _ : SSet.t = check t in
  ()

(* Pretty-printing *)

let rec pp fmt = function
  | Chars cset -> (
      let intervals = (cset : Cset.t :> (int * int) list) in
      let cp fmt c =
        if c >= 32 && c <= 126 then Format.fprintf fmt "'%c'" (Char.chr c)
        else Format.fprintf fmt "0x%04X" c
      in
      match intervals with
        | [(c, c')] when c = c' -> cp fmt c
        | _ ->
            Format.fprintf fmt "[%a]"
              (Format.pp_print_list
                 ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ")
                 (fun fmt (lo, hi) ->
                   if lo = hi then cp fmt lo
                   else Format.fprintf fmt "%a-%a" cp lo cp hi))
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
