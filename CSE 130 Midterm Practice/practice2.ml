(*write down : 
(a) If the expression is accepted, the value bound ("fn" for functions) 
    and its type,

(b) If the expression is rejected due to a type error, "type error",

(c) If the expression is rejected due to an unbound variable,
    the name of the variable that is not bound. 
    Recall that if a type error occurs, then the 
    variable binding does not happen. Check your 
    answers by entering this sequence into the interpreter.*)

let a =
  let x = 20 in 
    let y = 
    let x = 5 in
      x + x 
  in
  x + y;;
  
  val a : int = 30


(* scopes are dangerous be careful! *)
let b = 
    let x = "ab" in
    let y = (let x = "cd" in x) ^ x in
      x ^ y
    ;;
 
  val b : string = "abcdab"


let c = 
    let x = 22 in
      x::y
    ;;
Error: Unbound value y


let rec f x = if x > 0 then x :: (f (x-2)) else 0;;
  type error - has int, but expected int list

let g x = x * a;;
  val g : int -> int = <fun>

let a = -1 ;;
  val a : int = -1

let f x = 
    let a = 20 in 
      a + (g x)
  ;;
  val f : int -> int = <fun>

let z = (f 5) * a ;;
  val z : int = -170

(*
  (f 5)      * -1 
  (a + g 5)  * -1
  (20 + 150) * -1
  (170)      * -1
  -170 
*)