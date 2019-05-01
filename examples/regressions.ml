(* This test that unicode_old.ml is a strict sub-set of 
 * new unicode.ml. *)

let test_versions = ("6.3.0","12.1.0")

let regressions = [
  ("lo",[(0x13a0,0x13f4);
         (0x10d0,0x10fa);
         (0x10fd,0x10ff);
         (0x1885,0x1886)]);
  ("mc",[(0x19b0,0x19c0);
         (0x19c8,0x19c9);
         (0x1baa,0x1baa);
         (0x1bac,0x1bad);
         (0x1cf2,0x1cf3);
         (0xa9bd,0xa9c0)]);
  ("po",[(0x166d,0x166e)]);
  ("other_alphabetic", [(0x19b0,0x19c0);
                        (0x19c8,0x19c9);
                        (0x1cf2,0x1cf3)])
]

let interval s e =
  Array.to_list
    (Array.init (e-s) (fun pos -> s + pos))

exception Found

let test_exception name x =
  try
    let l = List.assoc name regressions in
    List.iter (fun (s,e) ->
      if s<=x && x<=e then raise Found) l
  with Not_found -> ()

let compare name old_l new_l =
  let code_points = List.fold_left (fun res (s,e) ->
    res@(interval s e)) [] old_l
  in
  let test x =
    try
      test_exception name x;
      List.iter (fun (s,e) ->
        if s<=x && x<=e then raise Found) new_l;
      false
    with Found -> true
  in
  List.iter (fun x ->
    if not (test x) then
      Printf.printf "Code point 0x%x missing in %s!\n" x name)
             code_points

let test new_l (name, old_l) =
  (* Cn is for unassigned code points, which are allowed to be
   * used in future version. *)
  if name <> "cn" then
    compare name old_l (List.assoc name new_l)

let () =
  if (Unicode_old.version,Sedlex_ppx.Unicode.version) <> test_versions then
    failwith (Printf.sprintf "Test written for versions: %s => %s\n%!" Unicode_old.version Sedlex_ppx.Unicode.version);
  Printf.printf "Testing Unicode regression: %s => %s\n%!" Unicode_old.version Sedlex_ppx.Unicode.version;
  List.iter (test Sedlex_ppx.Unicode.Categories.list) Unicode_old.Categories.list;
  List.iter (test Sedlex_ppx.Unicode.Properties.list) Unicode_old.Properties.list
