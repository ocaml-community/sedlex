exception MalFormed

let width = Array.make 256 (-1)
let () =
  for i = 0 to 127 do width.(i) <- 1 done;
  for i = 192 to 223 do width.(i) <- 2 done;
  for i = 224 to 239 do width.(i) <- 3 done;
  for i = 240 to 248 do width.(i) <- 4 done


let next s i =
  match s.[i] with
    | '\000'..'\127' as c ->
        Char.code c
    | '\192'..'\223' as c ->
        ((Char.code c - 192) lsl 6) lor
        ((Char.code s.[i+1] - 128))
    | '\224'..'\239' as c ->
        ((Char.code c - 192) lsl 12) lor
        ((Char.code s.[i+1] - 128) lsl 6) lor
        ((Char.code s.[i+2] - 128))
    | '\240'..'\248' as c ->
        ((Char.code c - 192) lsl 18) lor
        ((Char.code s.[i+1] - 128) lsl 12) lor
        ((Char.code s.[i+2] - 128) lsl 6) lor
        ((Char.code s.[i+3] - 128))
    | _ -> raise MalFormed

let compute_len s pos bytes =
  let rec aux n i =
    if i >= pos + bytes then if i = pos + bytes then n else raise MalFormed
    else 
      let w = width.(Char.code s.[i]) in
      if w > 0 then aux (succ n) (i + w) 
      else raise MalFormed
  in
  aux 0 pos

let rec blit_to_int s spos a apos n =
  if n > 0 then begin
    a.(apos) <- next s spos;
    blit_to_int s (spos + width.(Char.code s.[spos])) a (succ apos) (pred n)
  end

let to_int_array s pos bytes =
  let n = compute_len s pos bytes in
  let a = Array.create n 0 in
  blit_to_int s pos a 0 n;
  a

(**************************)

let width_code_point p =
  if p <= 127 then 1
  else if p <= 0x7ff then 2
  else if p <= 0xffff then 3
  else if p <= 0x10ffff then 4
  else raise MalFormed

(* Adapted from Netstring *)
let store p s i =
  if p <= 127 then (
    s.[i] <- Char.chr p;
    i + 1
  )
  else if p <= 0x7ff then (
    s.[i] <- Char.chr (0xc0 lor (p lsr 6));
    s.[i+1] <- Char.chr (0x80 lor (p land 0x3f));
    i+2
  )
  else if p <= 0xffff then (
    (* Refuse writing surrogate pairs, and fffe, ffff *)
    if (p >= 0xd800 & p < 0xe000) or (p >= 0xfffe) then raise MalFormed
      failwith "Encodings.Utf8.store";
    s.[i] <- Char.chr (0xe0 lor (p lsr 12));
    s.[i+1] <- Char.chr (0x80 lor ((p lsr 6) land 0x3f));
    s.[i+2] <- Char.chr (0x80 lor (p land 0x3f));
    i+3
  )
  else if p <= 0x10ffff then (
    s.[i] <- Char.chr (0xf0 lor (p lsr 18));
    s.[i+1] <- Char.chr (0x80 lor ((p lsr 12) land 0x3f));
    s.[i+2] <- Char.chr (0x80 lor ((p lsr 6)  land 0x3f));
    s.[i+3] <- Char.chr (0x80 lor (p land 0x3f));
    i+4
  )
  else raise MalFormed

let buf = ref (String.create 1024)

let from_int_array a apos len =
  if (String.length !buf < len * 4) then buf := String.create (len * 4);
  let rec aux apos spos len =
    if len > 0 then aux (succ apos) (store a.(apos) !buf spos) (pred len)
    else String.sub !buf 0 spos in
  aux apos 0 len
