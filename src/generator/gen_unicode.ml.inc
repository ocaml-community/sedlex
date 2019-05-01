(* This file generates unicode data from
 * the files exported at https://www.unicode.org/Public/<unicode version>
 * and stored at src/generator/data. *)

let target = Sys.argv.(1)

let categories =
  Hashtbl.create 1024

let labels =
  Hashtbl.create 1024

(* Categories and Properties that we're keeping. *) 
let keepers =
  ["cc"; "cf"; "cn"; "co"; "cs"; "ll"; "lm"; "lo"; "lt"; "lu"; "mc"; "me";
   "mn"; "nd"; "nl"; "no"; "pc"; "pd"; "pe"; "pf"; "pi"; "po"; "ps"; "sc";
   "sk"; "sm"; "so"; "zl"; "zp"; "zs"; "alphabetic"; "ascii_hex_digit";
   "hex_digit"; "id_continue"; "id_start"; "lowercase"; "math";
   "other_alphabetic"; "other_lowercase"; "other_math"; "other_uppercase";
   "uppercase"; "white_space"; "xid_continue"; "xid_start"]

let prop_interval_rex =
  Str.regexp "^\\([0-9a-fA-F]+\\)\\.\\.\\([0-9a-fA-F]+\\)[ ]*;[ ]+\\([a-zA-Z_]+\\)[ ]+#[ ]+\\([a-zA-Z][a-zA-Z&]\\)"

let prop_single_rex =
  Str.regexp "^\\([0-9a-fA-F]+\\)[ ]*;[ ]+\\([a-zA-Z_]+\\)[ ]+#[ ]+\\([a-zA-Z][a-zA-Z&]\\)"

let derived_interval_rex =
  Str.regexp "^\\([0-9a-fA-F]+\\)\\.\\.\\([0-9a-fA-F]+\\)[ ]*;[ ]+\\([a-zA-Z_]+\\)"

let derived_single_rex =
  Str.regexp "^\\([0-9a-fA-F]+\\)[ ]*;[ ]+\\([a-zA-Z_]+\\)"

let add_entry hashtbl (b,e) name =
  let mk s =
    int_of_string (Printf.sprintf "0x%s" s)
  in
  let interval = (mk b, mk e) in
  let label = String.lowercase_ascii name in
  if List.mem label keepers then
    Hashtbl.add hashtbl label interval

let match_interval s =
  if (Str.string_match prop_interval_rex s 0) then
    let interval = Str.matched_group 1 s, Str.matched_group 2 s in
    add_entry labels interval (Str.matched_group 3 s);
    add_entry categories interval (Str.matched_group 4 s)

let match_single s =
  if (Str.string_match prop_single_rex s 0) then
    let interval = Str.matched_group 1 s, Str.matched_group 1 s in
    add_entry labels interval (Str.matched_group 2 s);
    add_entry categories interval (Str.matched_group 3 s)

let match_derived_interval s =
  if (Str.string_match derived_interval_rex s 0) then
    let interval = Str.matched_group 1 s, Str.matched_group 2 s in
    add_entry categories interval (Str.matched_group 3 s)

let match_derived_single s =
  if (Str.string_match derived_single_rex s 0) then
    let interval = Str.matched_group 1 s, Str.matched_group 1 s in
    add_entry categories interval (Str.matched_group 2 s)

let split list n =
  let rec aux acc rem =
    match acc, rem with
      | [], el::rem ->
          aux [[el]] rem
      | l::acc, el::rem when List.length l = n ->
          aux ([el]::(List.rev l)::acc) rem
      | l::acc, el::rem ->
          aux ((el::l)::acc) rem
      | _, [] -> List.rev acc
  in
  aux [] list

let print_elements ch hashtbl =
  let cats =
    List.sort_uniq Pervasives.compare
      (Hashtbl.fold (fun cat _ l -> cat::l) hashtbl [])
  in
  let len = List.length cats in
  List.iter (fun c ->
    let entries =
      List.map (fun (b,e) -> Printf.sprintf "0x%x, 0x%x" b e)
        (List.sort_uniq Pervasives.compare
          (Hashtbl.find_all hashtbl c))
    in
    let entries =
      List.map (String.concat "; ") (split entries 5)
    in
    let entries =
      String.concat ";\n     " entries
    in
    Printf.fprintf ch "  let %s =\n    [%s]\n\n" c entries) cats;
  Printf.fprintf ch "  let list = [\n";
  List.iteri (fun pos c ->
    Printf.fprintf ch "    (%S, %s)%s\n" c c (if pos == len - 1 then "" else ";"))
             cats;
  Printf.fprintf ch "  ]\n\n"

let files = [
  ("PropList.txt", [match_interval; match_single]);
  ("DerivedCoreProperties.txt", [match_interval; match_single]);
  ("DerivedGeneralCategory.txt", [match_derived_interval; match_derived_single])
]

let read_version fname =
  let version_rex =
    Str.regexp "^# PropList-\\([0-9]+\\.[0-9]+\\.[0-9]+\\)\\.txt"
  in
  let ch = open_in_bin fname in
  let s = input_line ch in
  close_in ch;
  ignore(Str.string_match version_rex s 0);
  Str.matched_group 1 s

let () =
  let base_dir =
    Filename.concat (Filename.dirname Sys.executable_name) "data"
  in
  let version =
    read_version (Filename.concat base_dir "PropList.txt")
  in
  List.iter (fun (fname, fns) ->
    let ch =
      open_in_bin
        (Filename.concat base_dir fname)
    in
    try
      while true do
        let ret = input_line ch in
        List.iter (fun fn -> fn ret) fns 
      done
    with End_of_file -> close_in ch) files;
  let ch = open_out_bin target in
  Printf.fprintf ch "let version = %S\n\n" version;
  Printf.fprintf ch "module Categories = struct\n\n";
  print_elements ch categories;
  Printf.fprintf ch "end\n\n";
  Printf.fprintf ch "module Properties = struct\n\n";
  print_elements ch labels;
  Printf.fprintf ch "end\n";
  close_out ch
