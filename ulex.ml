type node = { 
  id : int; 
  mutable eps : node list; 
  mutable trans : (Cset.t * node) list;
}

type regexp = node -> node

let cur_id = ref 0
let new_node () =
  incr cur_id;
  { id = !cur_id; eps = []; trans = [] }

let seq r1 r2 succ = r1 (r2 succ)

let alt r1 r2 succ =
  let n = new_node () in
  n.eps <- [r1 succ; r2 succ];
  n

let rep r succ =
  let n = new_node () in
  n.eps <- [r n; succ];
  n

let eps succ = succ

let chars c succ =
  let n = new_node () in
  n.trans <- [c,succ];
  n


type state = node list

let rec add_node state node = 
  if List.memq node state then state else add_nodes (node::state) node.eps
and add_nodes state nodes =
  List.fold_left add_node state nodes


let transition state =
  (* Merge transition with the same target *)
  let rec norm = function
    | (c1,n1)::((c2,n2)::q as l) ->
	if n1 == n2 then norm ((Cset.union c1 c2,n1)::q)
	else (c1,n1)::(norm l)
    | l -> l in
  let t = List.concat (List.map (fun n -> n.trans) state) in
  let t = norm (List.sort (fun (c1,n1) (c2,n2) -> n1.id - n2.id) t) in

  (* Split char sets so as to make them disjoint *)
  let rec split (all,t) ((c0 : Cset.t),n0) = 
    let t = 
      [(Cset.difference c0 all, [n0])] @
      List.map (fun (c,ns) -> (Cset.intersection c c0, n0::ns)) t @
      List.map (fun (c,ns) -> (Cset.difference c c0, ns)) t in
    (Cset.union all c0,
    List.filter (fun (c,ns) -> not (Cset.is_empty c)) t) in

  let (_,t) = List.fold_left split (Cset.empty,[]) t in

  (* Epsilon closure of targets *)
  let t = List.map (fun (c,ns) -> (c,add_nodes [] ns)) t in
  t


let c i j = chars (Cset.interval i j)
let () =
  let re = 
    rep (
      alt (seq (c 100 200) (c 100 200))
      (seq (c 150 250) (c 200 300)))
  in
  let final = new_node () in
  let entry = add_node [] (re final) in
  let trans = transition entry in
  List.iter 
    (fun (c,ns) ->
      List.iter (fun (i,j) ->
	print_int i;
	print_char '-';
	print_int j;
	print_char ' ') c;
      print_char ':';
      List.iter (fun n ->
	print_int n.id;
	print_char ' ') ns
    ) trans;
  print_endline ";";
	
