exception MLFailure of string

type binop = 
      Plus 
    | Minus 
    | Mul 
    | Div 
    | Eq 
    | Ne 
    | Lt 
    | Le 
    | And 
    | Or          
    | Cons

type expr =   
      Const of int 
    | True   
    | False      
    | NilExpr
    | Var of string    
    | Bin of expr * binop * expr 
    | If  of expr * expr * expr
    | Let of string * expr * expr 
    | App of expr * expr 
    | Fun of string * expr    
    | Letrec of string * expr * expr

type value =  
      Int of int		
    | Bool of bool          
    | Closure of env * string option * string * expr 
    | Nil                    
    | Pair of value * value     

and env = (string * value) list

let binopToString op = 
  match op with
      Plus -> "+" 
    | Minus -> "-" 
    | Mul -> "*" 
    | Div -> "/"
    | Eq -> "="
    | Ne -> "!="
    | Lt -> "<"
    | Le -> "<="
    | And -> "&&"
    | Or -> "||"
    | Cons -> "::"

let rec valueToString v = 
  match v with 
      Int i -> 
        Printf.sprintf "%d" i
    | Bool b -> 
        Printf.sprintf "%b" b
    | Closure (evn,fo,x,e) -> 
        let fs = match fo with None -> "Anon" | Some fs -> fs in
          Printf.sprintf "{%s,%s,%s,%s}" (envToString evn) fs x (exprToString e)
    | Pair (v1,v2) -> 
        Printf.sprintf "(%s::%s)" (valueToString v1) (valueToString v2) 
    | Nil -> 
        "[]"

and envToString evn =
  let xs = List.map (fun (x,v) -> Printf.sprintf "%s:%s" x (valueToString v)) evn in
    "["^(String.concat ";" xs)^"]"

and exprToString e =
  match e with
    | Const i -> Printf.sprintf "%d" i
    | True -> "true" 
    | False -> "false"
    | Var x -> x
    | Bin (e1,op,e2) -> 
        Printf.sprintf "%s %s %s" 
          (exprToString e1) (binopToString op) (exprToString e2)
    | If (e1,e2,e3) -> 
        Printf.sprintf "if %s then %s else %s" 
          (exprToString e1) (exprToString e2) (exprToString e3)
    | Let (x,e1,e2) -> 
        Printf.sprintf "let %s = %s in \n %s" 
          x (exprToString e1) (exprToString e2) 
    | App (e1,e2) -> 
        Printf.sprintf "(%s %s)" (exprToString e1) (exprToString e2)
    | Fun (x,e) -> 
        Printf.sprintf "fun %s -> %s" x (exprToString e) 
    | Letrec (x,e1,e2) -> 
        Printf.sprintf "let rec %s = %s in \n %s" 
          x (exprToString e1) (exprToString e2) 

(*********************** Some helpers you might need ***********************)

let rec fold f base args = 
  match args with [] -> base
                | h::t -> fold f (f(base,h)) t

let listAssoc (k,l) = 
  fold (fun (r,(t,v)) -> if r = None && k=t then Some v else r) None l

(*********************** Your code starts here ****************************)

(*
Finds the first binding to the left
If no binding is found raise(MLFailure "not found")

val lookup: 'a * ('a * 'b) list -> 'b 
*)
let lookup (x,evn) = 
  match listAssoc(x,evn) with
    | Some x  ->  x
    | _       ->  raise( MLFailure ("variable not bound: "^x) )

(*
val eval : env * expr -> value 
evaluates e in the environment evn 
*)
let rec eval (evn,e) = match e with
  | Const x       ->  Int x
  | Var x		->  lookup(x,evn)
  | True		->  Bool true
  | False		->  Bool false
  | NilExpr	->  Nil
  | Bin (e1,op,e2)  ->  
      begin match (eval(evn,e1),op,eval(evn,e2)) with
        | Int x, Plus,  Int y ->  Int   (x + y)
        | Int x, Minus, Int y ->  Int   (x - y)
        | Int x, Mul,   Int y ->  Int   (x * y)
        | Int x, Div,   Int y ->  Int   (x / y)
        | Int x, Eq,    Int y ->  Bool  (x = y) 
        | Int x, Ne,    Int y ->  Bool  (x <> y) (* != is negation of == *)
        | Int x, Lt,    Int y ->  Bool  (x < y)
        | Int x, Le,    Int y ->  Bool  (x <= y)
        | Bool x, Eq,   Bool y -> Bool  (x = y)
        | Bool x, Ne,   Bool y -> Bool  (x != y)
        | Bool x, And,  Bool y ->  Bool  (x && y)
        | Bool x, Or,   Bool y ->  Bool  (x || y) 
        | Int  x, Cons, y      -> Pair(Int x,y) (*Const 1 to Cons to Bin(--)*)
        | _ ->  raise(MLFailure("Invalid Operands"))
      end
  | If (e1,e2,e3) ->  
      if eval(evn,e1) = Bool true 
      then eval(evn,e2) 
      else eval(evn,e3)
  | Let (x,e1,e2) ->  eval((x,eval(evn,e1))::evn,e2)
  | Letrec (x,e1,e2) -> 
      let evn2 = 
        match eval(evn, e1) with
          | Closure(evn3,None,a,e) -> Closure(evn3,Some x,a,e)
          | _			 -> eval(evn,e1)
      in eval(((x,evn2)::evn),e2)
  | App (e1,e2) -> 
      if Var("hd") = e1 then (match eval(evn,e2) with Pair(x,y) -> x)
      else 
      if Var("tl") = e1 then (match eval(evn, e2) with Pair(x,y) -> y)
      else
        let Closure(a,b,c,d) = eval(evn,e1) in
          if b = None 
          then eval(((c,eval(evn,e2))::a),d)
          else eval(((c,eval(evn,e2))::evn),d)
  | Fun (x,e) -> Closure(evn, None, x, e)
  | _ -> raise (MLFailure "Invalid Operands")

(**********************     Testing Code  ******************************)
(*

let evn = [("z1",Int 0);("x",Int 1);("y",Int 2);("z",Int 3);("z1",Int 4)]

let e1  = Bin(Bin(Var "x",Plus,Var "y"), Minus, Bin(Var "z",Plus,Var "z1"))

let _   = eval (evn, e1)        
(* EXPECTED: Nano.value = Int 0 *)
(*
let _   = eval (evn, Var "p")   
(* EXPECTED:  Exception: Nano.MLFailure "variable not bound: p". 
*)
*)
(* Uncomment to test part (b) *)

let evn = [("z1",Int 0);("x",Int 1);("y",Int 2);("z",Int 3);("z1",Int 4)]

let e1  = If(Bin(Var "z1",Lt,Var "x"),Bin(Var "y",Ne,Var "z"),False)

let _   = eval (evn,e1)         (* EXPECTED: Nano.value = Bool true *)

let e2  = If(Bin(Var "z1",Eq,Var "x"), 
Bin(Var "y",Le,Var "z"),
Bin(Var "z",Le,Var "y")
)

let _   = eval (evn,e2)         (* EXPECTED: Nano.value = Bool false *)


(* Uncomment to test part (c) *)

let e1 = Bin(Var "x",Plus,Var "y")

let e2 = Let("x",Const 1, Let("y", Const 2, e1)) 

let _  = eval ([], e2)          (* EXPECTED: Nano.value = Int 3 *)

let e3 = Let("x", Const 1, 
Let("y", Const 2, 
Let("z", e1, 
Let("x", Bin(Var "x",Plus,Var "z"), 
e1)
)
)
)

let _  = eval ([],e3)           
(* EXPCETED: Nano.value = Int 6 *)

(* Uncomment to test part (d) *)

let _ = eval ([], Fun ("x",Bin(Var "x",Plus,Var "x"))) 

(* EXPECTED: Nano.value = Closure ([], None, "x", Bin (Var "x", Plus, Var "x"))*)

let _ = eval ([],App(Fun ("x",Bin(Var "x",Plus,Var "x")),Const 3));;

(* EXPECTED: Nano.value = Int 6 *)

let e3 = Let ("h", Fun("y",Bin(Var "x", Plus, Var "y")),App(Var "f",Var "h"));;

let e2 = Let("x",Const 100,e3);;

let e1 = Let("f",Fun("g",Let("x",Const 0,App(Var "g",Const 2))),e2);;

let _  = eval ([],e1);;
(* EXPECTED: Nano.value = Int 102 *)

let _ = eval ([],Letrec("f",Fun("x",Const 0),Var "f"));;
(* EXPECTED: Nano.value = Closure ([], Some "f", "x", Const 0) *)

(* Uncomment to test part (e)*)
let _ = eval ([],Letrec("fac",Fun("n", If (Bin (Var "n", Eq, Const 0),Const 1, Bin(Var "n", Mul, App(Var "fac",Bin(Var "n",Minus,Const 1))))), App(Var "fac", Const 10)))

(* EXPECTED: Nano.value = Int 3628800 *)


(* Uncomment to test part (f)*)

let _ = eval ([],Bin(Const 1,Cons,Bin(Const 2,Cons,NilExpr)))

(* EXPECTED: Nano.value = Pair (Int 1, Pair (Int 2, Nil)) *)

let _ = eval ([],App(Var "hd",Bin(Const 1,Cons,Bin(Const 2,Cons,NilExpr))))

(* EXPECTED: Nano.value = Int 1 *)

let _ = eval ([],App(Var "tl",Bin(Const 1,Cons,Bin(Const 2,Cons,NilExpr))))

(* EXPECTED: Nano.value = Pair (Int 2, Nil) *)

*)
