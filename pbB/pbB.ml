open F_parser
(* 
  Problema B - Realizado por:
   nº45856 - Claudio Redondo
   nº48184 - Samuel Dias
*)
(* Desenvolvimento da Formula Normal Conjuntiva (FNC)
   - Implfree - Remove as implicações
   - NNFC - Remove as duplas negações
   - CNFC - Usa a função distr para fazer a distributiva e completar a conversão para FNC
  
Retirado dos slides das aulas práticas

*)
let rec implfree var = match var with
| Var x -> Var x
| Not x -> Not (implfree x)
| Or (x,y) -> Or(implfree x,implfree y)
| And (x,y) -> And(implfree x,implfree y)
| Implies (x,y) -> Or(Not(implfree x),implfree y)
| Equiv (x,y) -> implfree (And(Implies(x, y), Implies(y, x)))
|a->a

let rec nnfc var = match var with (* NNFC -> Remove as duplas negações*)
|Var x -> Var x
|Not (Not x) -> nnfc (x)
|Not(And(x,y)) -> Or(nnfc (Not(x)),nnfc (Not(y)))
|Not(Or(x,y)) -> And(nnfc (Not(x)),nnfc (Not(y)))
|Or(x,y) -> Or(nnfc x,nnfc y)
|And(x,y) -> And(nnfc x,nnfc y)
|Not (False)->True
|Not (True)->False
|a->a

let rec distr x y = match (x,y) with 
|And (w,z), c -> And (distr w c, distr z c)
|c , And (w,z) -> And (distr c w , distr c z)
|w,z -> Or (w,z)


let rec cnfc var = match var with
| Var x -> Var x
| Or (x, y) -> distr (cnfc (x)) (cnfc (y))
| And (x,y) -> And(cnfc (x),cnfc (y))
|a->a



(* Função básica para simplificar e transformar a formula recebida através do F_Parser em uma FNC *)
let fnc formula_var = cnfc(nnfc(implfree (formula_var)))

(* Função que vai nega uma possivel clausula*)
let negar_clausula (literal, boolean) = (literal, not boolean) 

(*
Retirar as clausulas da FNC
   - Se a clausula for positiva, vai ser do tipo (x, true)
   - Se a clausula for negativa, vai ser do tipo (x, false)
   - Para negar, usamos a função negar clausula, usando o List map para negar cada clausula, caso seja necessário negar
   - No caso do AND, concatena as duas ou mais listas numa só
   - No caso do OR, procura o primeiro elemento das listas e concatena-os numa só
   - Para o True função retorna uma lista contendo uma única cláusula com um literal especial
   - Para o Falso a função retorna uma lista contendo uma única cláusula vazia ([ [ ] ]).

   
*)
let rec clauses var = 
  match var with 
|Var x -> [ [ (x, true)] ]
|Not (Var x) -> [ [ (x,false) ] ]
|Not f -> List.map(List.map negar_clausula) (clauses f) 
|And (f,g) -> List.concat [clauses f; clauses g]
|Or (f,g) -> [List.hd (clauses f) @ List.hd (clauses g)]
|True -> [ [("*", true)] ]
|False -> [ [] ]
|_ -> []

(*
- remove_sublists_with_literal: 
A função recebe uma lista de cláusulas (list) e um literal (literal) e remove as cláusulas que contêm o literal especificado. 
Ela percorre cada cláusula na lista e verifica se o literal ou um literal especial ("*", verdadeiro) está presente na cláusula. 
Se o literal estiver presente, a cláusula é "eliminada". 
Chamamos novamente para executar até que a lista esteja vazia. 
O resultado é uma nova lista contendo apenas as cláusulas que não possuem o literal especificado.

- remove_literal: 
A função recebe uma lista de cláusulas (list) e um literal (literal) e remove todas as ocorrências desse literal em cada cláusula. 
Ela utiliza List.map para percorrer cada cláusula da lista e, em seguida, utiliza List.filter para remover o literal especificado de cada cláusula.
A função List.filter recebe uma função anônima que compara cada elemento da cláusula com o literal e retorna true se eles forem diferentes. 
O resultado é uma nova lista com as cláusulas onde o literal foi removido.

- simplify_list: 
Execução das duas funções referidas anteriormente, para simplificar processos.


literais_sublistas_unitarias: 
A função recebe uma lista de cláusulas (list) e retorna uma lista contendo os literais que ocorrem em subcláusulas unitárias. 
Usa uma função auxiliar recursiva aux que percorre cada cláusula da lista. 
Se a cláusula tiver apenas um literal, esse literal é adicionado à lista acc. 
Caso contrário, a função continua a executar nas as próximas cláusulas. 
No final, a lista acc é invertida (List.rev acc) para obter a lista final de literais de subcláusulas unitárias   
*)
let rec remove_sublists_with_literal list literal =
  match list with
  | [] -> []
  | hd :: tail ->
    if List.mem literal hd || List.mem ("*", true) hd then
      remove_sublists_with_literal tail literal
    else
      hd :: remove_sublists_with_literal tail literal


let rec remove_literal list literal =
  List.map (fun sublist -> List.filter (fun str -> str <> literal) sublist) list 



let simplify_list lit list =
  let list_lit = remove_sublists_with_literal list lit in
  let not_lit = negar_clausula lit in
  remove_literal list_lit not_lit

let literais_sublistas_unitarias list =
  let rec aux acc = function
    | [] -> List.rev acc
    | hd::tl ->
      if List.length hd = 1 then
        aux (List.hd hd :: acc) tl
      else
        aux acc tl
  in
  aux [] list

  (* Unit Propagate - Realiza a simplificação das clausulas 
A função recebe uma lista de cláusulas (list) como entrada.
Existem dois casos principais a considerar:
Se a lista estiver vazia ([]), isso significa que todas as cláusulas foram satisfeitas e não há mais nada a fazer. Nesse caso, a função retorna uma lista vazia ([]).
Caso contrário, a função continua com a próxima etapa de propagação de unidades, verificando se a lista contém uma cláusula vazia (List.mem [] list). 
Se isso acontecer, significa que uma cláusula vazia foi encontrada e, portanto, a função retorna uma lista contendo uma cláusula vazia ([[]]).
Se a lista não contiver uma cláusula vazia, a função continua a sua execução.
A função literais_sublistas_unitarias é chamada para obter uma lista dos literais que ocorrem em subcláusulas unitárias presentes na lista de cláusulas.
Se a lista de literais de subcláusulas unitárias (lista_clausulas_unitarias) estiver vazia, isso significa que não existem subcláusulas unitárias na lista. 
Nesse caso, a função retorna a lista original (list) sem fazer alterações adicionais.
Caso contrário, a função continua com a execução. 
A função simplify_list é chamada para simplificar a lista de cláusulas, removendo as cláusulas que contêm subcláusulas com os literais de lista_clausulas_unitarias.
A primeira clausula (List.hd lista_clausulas_unitarias) vai ser usada para simplificar a lista de cláusulas.
A função unit_propagate é chamada recursivamente com a lista de cláusulas simplificada, até ao fim da simplificação
  *)
  
let rec unit_propagate list =
  match list with
  | [] -> [] 
  | _  ->
    if not (List.mem [] list) then
    let lista_clausulas_unitarias = literais_sublistas_unitarias list in
    if lista_clausulas_unitarias = [] then list else 
    if not (List.mem [] list) then 
      unit_propagate (simplify_list (List.hd lista_clausulas_unitarias) list) else
        list 
      else [ [] ]
      
    
let firstelement list = List.hd (List.hd list)
    (*
O algoritmo DPLL sobre uma entrada S (a formula por analisar em forma de conjunto de conjuntos de literais) define-se da seguinte forma:
DPLL(S)
unit_propagate(S)
se S = {} então devolver SAT
senão se {} ∈ S ent˜ao devolver UNSAT senão
l é um literal qualquer retirado de S
se DPLL(S|l) = SAT então devolver SAT
caso contrário devolver DPLL(S|−l) 
*)
let rec dpll list =
  if List.length list = 1 && List.hd list <> [] then true else
  let unit_prop_list = unit_propagate list in
  match unit_prop_list with
  | [] -> true (* SAT *)
  | [[]] -> false (* UNSAT *)
  | _ ->
    let newlit = firstelement unit_prop_list in
    let notnewlit = negar_clausula newlit in
    if dpll (simplify_list newlit unit_prop_list) then true (*SAT*)
    else dpll (simplify_list notnewlit unit_prop_list)

(* Função Output*)
let () =
let input = Option.get (parse "stdin") in
    let result = dpll (clauses (fnc input)) in
    (* let () = print_clauses (clauses (fnc input)) in *)
    if result = true then
      print_endline "SAT"
    else if result = false then
      print_endline "UNSAT"


(* Exemplo de execução 
   
Formula = (!X1 & (!X2 & !X3)) & ((!X2) <-> (X3 & (X2 & X1)))
Formula em FNC = And(And(Not (Var "X1"), And(Not (Var "X2"), Not (Var "X3"))), Or(Not (Var "X2"), And(Not (Var "X3"), And(Not (Var "X2"), Not (Var "X1"))))))
Após passagem na função clauses = [[("X1", false); ("X2", false); ("X3", false)]; [("X2", false)]; [("X3", false)]; [("X3", true); ("X2", true); ("X1", true)]]
Após execução do dpll, e consequentemente pelo simplify_list e unit_propagate, obtemos: false
Então output: UNSAT
*)