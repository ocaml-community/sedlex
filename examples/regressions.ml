(* This test that unicode_old.ml is a strict sub-set of new unicode.ml. *)

module CSet = Sedlex_compiler.Cset
module Unicode = Sedlex_ppx.Unicode

let test_versions = ("17.0.0", "18.0.0")

let regressions =
  [ (* Example *)
    (* ("lt", CSet.union (CSet.singleton 0x1c5) (CSet.singleton (0x0001))) *) ]

let compare name (old_ : CSet.t) (new_ : CSet.t) =
  let diff = CSet.difference old_ new_ in
  let regressions =
    match List.assoc name regressions with
      | exception Not_found -> CSet.empty
      | x -> x
  in
  (* A regression is a code point of the old set that is expected to be
   * missing in the new set. *)
  let regressions_not_in_old = CSet.difference regressions old_ in
  let regressions_still_in_new = CSet.intersection regressions new_ in
  let diff = CSet.difference diff regressions in
  Seq.iter
    (fun x ->
      Printf.printf "Invalid regression for 0x%x in %s: absent in old set.\n" x
        name)
    (CSet.to_seq regressions_not_in_old);
  Seq.iter
    (fun x ->
      Printf.printf
        "Invalid regression for 0x%x in %s: still present in new set.\n" x name)
    (CSet.to_seq regressions_still_in_new);
  Seq.iter
    (fun x -> Printf.printf "Code point 0x%x missing in %s!\n" x name)
    (CSet.to_seq diff)

let test new_l (name, old_l) =
  (* Cn is for unassigned code points, which are allowed to be
   * used in future version. *)
  let old_l = Sedlex_compiler.Cset.to_list old_l in
  if name <> "cn" then (
    let old_l =
      List.fold_left
        (fun acc (a, b) -> CSet.union acc (CSet.interval a b))
        CSet.empty old_l
    in
    compare name old_l (List.assoc name new_l))

let () =
  if (Unicode_old.version, Unicode.version) <> test_versions then
    failwith
      (Printf.sprintf "Test written for versions: %s => %s\n%!"
         Unicode_old.version Unicode.version);
  Printf.printf "Testing Unicode regression: %s => %s\n%!" Unicode_old.version
    Unicode.version;
  List.iter (test Unicode.Categories.list) Unicode_old.Categories.list;
  List.iter (test Unicode.Properties.list) Unicode_old.Properties.list
