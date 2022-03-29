(* This test that unicode_old.ml is a strict sub-set of 
 * new unicode.ml. *)

let test_versions = ("13.0.0", "14.0.0")
let regressions = []
let interval s e = Array.to_list (Array.init (e - s) (fun pos -> s + pos))

exception Found

let test_exception name x =
  try
    let l = List.assoc name regressions in
    List.iter (fun (s, e) -> if s <= x && x <= e then raise Found) l
  with Not_found -> ()

let compare name old_l new_l =
  let code_points =
    List.fold_left (fun res (s, e) -> res @ interval s e) [] old_l
  in
  let test x =
    try
      test_exception name x;
      List.iter (fun (s, e) -> if s <= x && x <= e then raise Found) new_l;
      false
    with Found -> true
  in
  List.iter
    (fun x ->
      if not (test x) then
        Printf.printf "Code point 0x%x missing in %s!\n" x name)
    code_points

let test new_l (name, old_l) =
  (* Cn is for unassigned code points, which are allowed to be
   * used in future version. *)
  if name <> "cn" then compare name old_l (List.assoc name new_l)

let () =
  if (Unicode_old.version, Sedlex_ppx.Unicode.version) <> test_versions then
    failwith
      (Printf.sprintf "Test written for versions: %s => %s\n%!"
         Unicode_old.version Sedlex_ppx.Unicode.version);
  Printf.printf "Testing Unicode regression: %s => %s\n%!" Unicode_old.version
    Sedlex_ppx.Unicode.version;
  List.iter
    (test Sedlex_ppx.Unicode.Categories.list)
    Unicode_old.Categories.list;
  List.iter
    (test Sedlex_ppx.Unicode.Properties.list)
    Unicode_old.Properties.list
