(* Example of file when using the compiles mode *) 

open Lib
open Formulas
open Prop
open Bdd
open Logic

(* Pretty-printer for formulas, to be used with compiled mode *)
let print_formula fm = print_prop_formula fm; print_newline();;

let f = << ( 1 <=> 2 ) /\ ( 3 <=> 4 )>>;;
print_formula f;;

let taille = 100 in
let t = init_t taille in
let ht = init_ht taille in
let v = Logic.build t ht f in
debug_print_t t;
debug_print_h ht 10 10;
print_t t v "./img/main.dot";; 

