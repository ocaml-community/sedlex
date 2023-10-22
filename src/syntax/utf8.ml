let unsafe_byte s j = Char.code (String.unsafe_get s j)
let malformed s j l = `Malformed (String.sub s j l)

let width = function
  | '\000' .. '\127' -> 1
  | '\192' .. '\223' -> 2
  | '\224' .. '\239' -> 3
  | '\240' .. '\247' -> 4
  | _ -> 0

let r_utf_8 s j l =
  (* assert (0 <= j && 0 <= l && j + l <= String.length s); *)
  let uchar c = `Uchar (Uchar.unsafe_of_int c) in
  match l with
    | 1 -> uchar (unsafe_byte s j)
    | 2 ->
        let b0 = unsafe_byte s j in
        let b1 = unsafe_byte s (j + 1) in
        if b1 lsr 6 != 0b10 then malformed s j l
        else uchar (((b0 land 0x1F) lsl 6) lor (b1 land 0x3F))
    | 3 ->
        let b0 = unsafe_byte s j in
        let b1 = unsafe_byte s (j + 1) in
        let b2 = unsafe_byte s (j + 2) in
        let c =
          ((b0 land 0x0F) lsl 12) lor ((b1 land 0x3F) lsl 6) lor (b2 land 0x3F)
        in
        if b2 lsr 6 != 0b10 then malformed s j l
        else begin
          match b0 with
            | 0xE0 ->
                if b1 < 0xA0 || 0xBF < b1 then malformed s j l else uchar c
            | 0xED ->
                if b1 < 0x80 || 0x9F < b1 then malformed s j l else uchar c
            | _ -> if b1 lsr 6 != 0b10 then malformed s j l else uchar c
        end
    | 4 ->
        let b0 = unsafe_byte s j in
        let b1 = unsafe_byte s (j + 1) in
        let b2 = unsafe_byte s (j + 2) in
        let b3 = unsafe_byte s (j + 3) in
        let c =
          ((b0 land 0x07) lsl 18)
          lor ((b1 land 0x3F) lsl 12)
          lor ((b2 land 0x3F) lsl 6)
          lor (b3 land 0x3F)
        in
        if b3 lsr 6 != 0b10 || b2 lsr 6 != 0b10 then malformed s j l
        else begin
          match b0 with
            | 0xF0 ->
                if b1 < 0x90 || 0xBF < b1 then malformed s j l else uchar c
            | 0xF4 ->
                if b1 < 0x80 || 0x8F < b1 then malformed s j l else uchar c
            | _ -> if b1 lsr 6 != 0b10 then malformed s j l else uchar c
        end
    | _ -> assert false

let fold ~f acc s =
  let rec loop acc f s i last =
    if i > last then acc
    else (
      let need = width (String.unsafe_get s i) in
      if need = 0 then loop (f acc i (malformed s i 1)) f s (i + 1) last
      else (
        let rem = last - i + 1 in
        if rem < need then f acc i (malformed s i rem)
        else loop (f acc i (r_utf_8 s i need)) f s (i + need) last))
  in
  let pos = 0 in
  let len = String.length s in
  let last = pos + len - 1 in
  loop acc f s pos last
