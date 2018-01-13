open Bdd
open Formulas
open Prop

module Logic:

sig
  val make: tableT -> tableH -> variable -> id -> id -> id
  val apply_neg: tableT -> tableH -> id -> id
  val apply: tableT -> tableH -> op -> id -> id -> id
  val build: tableT -> tableH -> prop formula -> id
  val sat: id -> bool
  val valid: id -> bool
  val anysat: tableT -> id -> (variable * bool) list
  val allsat: tableT -> id -> (variable * bool) list list
end

  =

struct

  exception Unicite of string
  exception Utilite of string

  let make t ht v l h =
	if v < 1 then 
	  invalid_arg "Logic.make"
	else if (member ht v l h) then
	  lookup ht v l h 
	else if l == h then 
	  raise (Utilite "Test inutile")
	else 
	  let i = add t v l h in
	  (insert ht v l h i);
 	  i

  let rec apply_neg t ht i = match i with
	| 0 -> one
	| 1 -> zero
	| _ ->
	  try
		match (Hashtbl.find t.t i) with (v, l, h) ->
		  let nl = apply_neg t ht l in
		  let nh = apply_neg t ht h in
		  if (member ht v nl nh) then
			lookup ht v nl nh
		  else 
			make t ht i nl nh
	  with  
		Not_found -> raise (CleAbsente "dans la fonction apply_neg")

  let rec apply t ht o i1 i2 = match o with
	| Ou ->
	  if (i1 == one) || (i2 == one) then
		one
	  else if (i1 == zero) then
		i2
	  else if (i2 == zero) then
		i1
	  else 
		apply_aux t ht o i1 i2
	| Et ->
	  if (i1 == zero) || (i2 == zero) then
		zero
	  else if (i1 == one) then
		i2
	  else if (i2 == one) then
		i1
	  else
		apply_aux t ht o i1 i2
	| Impl ->
	  if (i1 == zero) then
		one
	  else if (i2 == one) then
		one
	  else if (i1 == one) && (i2 == zero) then
		zero
	  else 
		apply_aux t ht o i1 i2
	| Equiv ->
	  if (i1 == one) then
		i2
	  else if (i2 == one) then
		i1
	  else if (i1 == zero) then
		apply_neg t ht i2
	  else if (i2 == zero) then
		apply_neg t ht i1
	  else apply_aux t ht o i1 i2
  and apply_aux t ht o i1 i2 =
	let v1 = var t i1 in
	let l1 = low t i1 in
	let h1 = high t i1 in
	let v2 = var t i2 in
	let l2 = low t i2 in
	let h2 = high t i2 in
	if (v1 == v2) then
	  let o1 = apply t ht o l1 l2 in
	  let o2 = apply t ht o h1 h2 in
	  make t ht v1 o1 o2
	else if (v1 < v2) then
	  let o1 = apply t ht o l1 i2 in
	  let o2 = apply t ht o h1 i2 in
	  make t ht v1 o1 o2
	else 
	  let o1 = apply t ht o i1 l2 in
	  let o2 = apply t ht o i1 h2 in
	  make t ht v2 o1 o2

  let rec build t ht f = match f with
	| True -> one
	| False -> zero
	| Atom(P(i)) -> make t ht i one zero
	| Not(g) -> apply_neg t ht (build t ht g)
	| Or(g, h) -> apply t ht Ou (build t ht g) (build t ht h)
	| And(g, h) -> apply t ht Et (build t ht g) (build t ht h)
	| Imp(g, h) -> apply t ht Impl (build t ht g) (build t ht h)
	| Iff(g, h) -> apply t ht Equiv (build t ht g) (build t ht h)

  let sat i = not(i == zero)
  
  let valid i = i == one

  let anysat t i =
	let rec aux acc i = match i with
	  | 0 -> raise Not_found
	  | 1 -> acc
	  | _ ->
		try
		  match (Hashtbl.find t.t i) with
		  | (v, zero, h) -> aux ((v, true)::acc) h
		  | (v, l, _) -> aux ((v, false)::acc) l
		with
		  Not_found -> raise (CleAbsente "dans la fonction anysat")
	in
	aux [] i

  let rec allsat t i = match i with
	| 0 -> []
	| 1 -> [[]]
	| _ ->
	  try
		match (Hashtbl.find t.t i) with (v, l, h) ->
		  (List.map (fun l -> (v, true)::l) (allsat t l)) @ (List.map (fun l -> (v, false)::l) (allsat t h))
	  with
		Not_found -> raise (CleAbsente "dans la fonction allsay") 

end
