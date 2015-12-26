let x = 2 ;;
	val x : int = 2

let x = 2 + 3 - 4.0 ;;
	type error - expected int, but got float

let x = 2 + 3 ;;
	val x : int = 5

let x = "a" ;;
	val x : string = "a"

let x = (x,3) ;;
	val x : string * int = ("a", 3)

(* snd returns the second component of a pair *)
let y = ((snd x)^"b" , 2) ;;
	type error - expects string, but got int

(* fst returns the first component of a pair *)
let y = ((fst x)^"c" , 4) ;;
	val y : string * int = ("ac", 4)

let z = if (x = y) then (snd x) else (snd y) ;;
	val z : int = 4

type myrecord = {f1 : int; f2 : string ; f3 : int} ;;
let a = {f1 = x; f2 = (fst y); f3 = z} ;;
	type error - expects int, but was given string * int

let b = z::(snd y)::[] ;;
	val b : int list = [4; 4]

let c = (x;y;z) ;;
	val c : int = 4

let c = (x,y,z) ;;
	val c : (string * int) * (string * int) * int = (("a", 3), ("ac", 4), 4)

(* Why int list???? -> i think it is because it looks at b which is a list of ints *)
let m = if (snd x) = (snd y) then b else [] ;;
	val m : int list = []

let n = if (1 > 2) then ["a";"b"] else [] ;;
	val n : string list = []

let o = if (m = n) then 2 else 3 ;;
	type error - expected int list, but has sting list