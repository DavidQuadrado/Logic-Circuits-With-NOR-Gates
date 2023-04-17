(* ABRIR BIBLIOTECAS *)

open F_parser

(* LER INPUT*)

let input = parse "stdin" 

(* FUNÇÕES *)

(*transformar option-list para list*)
let rec exprecione x =
  match x with
  | Some exprecione -> exprecione
  | None -> []

(* Função que recebe uma formula e indica qual é a Varialvel mais "pequena lexicograficamente" *)

let sirChristopher form = 
  let vari = ref [|"Z"|] in  (* array que guarda Z com a Var mais "alta"  lexicograficamente *)
  let rec aux form =
  match form with
  | True          -> ()
  | False         -> ()
  | (Var a)        -> if a < !vari.(0) then !vari.(0) <- a (*acede ao array na posição 0 e depois se for menor substitui por A*)
  | Not (a)       -> aux a
  | And (a,b)     -> aux a; aux b (*percorre a arvore form até chegar as folhas que são as Var*)
  | Or (a,b)      -> aux a; aux b
  | Implies (a,b) -> aux a; aux b
  | Equiv (a,b)   -> aux a; aux b
in aux form; !vari.(0)

(* Função auxiliar (que nao funciona) para fazer a função "descodificador" *)

(*
let rec descodificadora equa =
  match equa with
  | True          -> "TRUE"
  | False         -> "FALSE"
  | (Var A)       -> A
  | Not (A)       -> A % A
  | And (A,B)     -> (A % A) % (B % B)
  | Or (A,B)      -> (A % B) % (A % B)
  | Implies (A,B) -> (((A % A) % B) % ((A % A) % B))
  | Equiv (A,B)   -> And ((A -> B)  (B -> A)) -> And ((((A % A) % B) % ((A % A) % B)),(((B % B) % A) % ((B % B) % A)))
                                              ->  (((((A % A) % B) % ((A % A) % B)) % (((A % A) % B) % ((A % A) % B))) 
                                                % ((((B % B) % A) % ((B % B) % A)) % (((B % B) % A) % ((B % B) % A))))
   *)

(* Função que ao receber uma formula irá transformar todos os operador em portas NOR *)

let rec descodificador equa minimal = (* minimal só irá funcionar para TRUE e FALSE em que minimal irá ser sempre o Z (neste caso) *)
  match equa with
  | False         -> "(" ^ minimal ^ " % " ^ "(" ^ minimal^ " % " ^ minimal ^")" ^ ")"  (*BOT*) 
  | True          -> descodificador ( Not (False) ) minimal                             (*TOP*)
  | (Var a)       -> a
  | Not (Or(a, b))-> "(" ^ descodificador a minimal ^ " % " ^ descodificador b minimal ^ ")"
  | Not (a)       -> "(" ^ descodificador a minimal ^ " % " ^ descodificador a minimal ^ ")"
  | And (a,b)     -> "(" ^ "(" ^ descodificador a minimal ^ " % " ^ descodificador a minimal ^ ")" ^ " % " ^ "(" ^ descodificador b minimal ^ " % " ^ descodificador b minimal ^ ")" ^ ")"
  | Or (a,b)      -> "(" ^ "(" ^ descodificador a minimal ^ " % " ^ descodificador b minimal ^ ")" ^ " % " ^ "(" ^ descodificador a minimal ^ " % " ^ descodificador b minimal ^ ")" ^ ")"
  | Implies (a,b) -> descodificador (Or (Not a,b ) ) minimal
  | Equiv (a,b)   -> descodificador (And ( (Implies (a,b)) , (Implies (b,a))) ) minimal
  (*  
  | Implies (a,b) -> "(" ^ "(" ^ "(" ^ descodificador a minimal ^ " % " ^ descodificador a minimal ^ ")" ^ " % " ^ descodificador b minimal ^ ")" ^ " % " ^
                     "(" ^ "(" ^ descodificador a minimal ^ " % " ^ descodificador a minimal ^ ")" ^ " % " ^ descodificador b minimal ^")" ^ ")"
  | Equiv (a,b)   -> "(" ^ "(" ^ "(" ^ "(" ^ "(" ^ descodificador a minimal ^ " % " ^ descodificador a minimal ^ ")" ^ 
  " % " ^ descodificador b minimal ^ ")" ^ " % " ^ "(" ^ "(" ^ descodificador a minimal ^ " % " ^ descodificador a minimal ^ ")" ^ " % " ^ descodificador b minimal ^ ")" ^ ")" ^
  " % " ^ "(" ^ "(" ^ "(" ^ descodificador a minimal ^ " % " ^ descodificador a minimal ^ ")" ^ " % " ^ descodificador b minimal ^ ")" ^ 
  " % " ^ "(" ^ "(" ^ descodificador a minimal ^ " % " ^ descodificador a minimal ^ ")" ^ " % " ^ descodificador b minimal ^ ")" ^ ")" ^ ")"                 (* A -> B *)
  ^ " % " ^ "(" ^ "(" ^ "(" ^ "(" ^ descodificador b minimal ^ " % " ^ descodificador b minimal ^ ")" ^ " % " ^ descodificador a minimal ^ ")" ^ 
  " % " ^ "(" ^ "(" ^ descodificador b minimal ^ " % " ^ descodificador b minimal ^ ")" ^ " % " ^ descodificador a minimal ^ ")" ^ ")" ^
  " % " ^ "(" ^ "(" ^ "(" ^ descodificador b minimal ^ " % " ^ descodificador b minimal ^ ")" ^ " % " ^ descodificador a minimal ^ ")" 
  ^ " % " ^ "(" ^ "(" ^ descodificador b minimal ^ " % " ^ descodificador b minimal ^ ")"  ^ " % " ^ descodificador a minimal ^ ")" ^ ")" ^ ")" ^ ")"       (* B -> A *)                                                                                                                                                
                                                                                                                                                            (* And ((A -> B)  (B -> A)) *)
*)

(* Função que irá receber uma lista de formulas  e depois irá descodificar usando a função SirChristopher e descodificador *)

let rec percorrelista lista = (* tranforma uma lista de formulas numa lista de strings*)
  match lista with
  |[] -> []
  |head :: tail -> descodificador (head) (sirChristopher head) :: percorrelista (tail);;

(* OUTPUT *)

List.iter (fun x -> Printf.printf "%s\n" x ) ( percorrelista (exprecione input)) (*percorre a lista e dá printf*)

(* COMO COMPILAR *)

(*eval $(opam env)*)
(* ocamlopt -I f_parser/ f_parser.cmxa ProblemaA.ml -o  abs *)