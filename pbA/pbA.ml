(* Lógica*)(*Abrir library F_parser*)
open F_parser

(* Variavél Global usada *)
let var_global = ref "Z"


(* A função transformação tem como objetivo realizar a transformação de false(bot) ou de true (top) seguindo as regras dadas no enunciado 
- A função vai realizar uma pesquisa intensiva, usando o ciclo for de 0 até ao fim da string dada no input procurando a letra com valor mais pequeno no 
código ASCII que pertenca à string, para isso foi preciso usar a library char para usar a função char.code que vai buscar o código ASCII mais pequeno presente na string,
isto desde que esteja entre o 'a' (65 na tabela ASCII) e o 'z' (90 na tabela ASCII)

- Caso esteja presente dentro dos limites, a função procede a  transformação da formula.

- É importante notar que a função transformacao não altera a fórmula original, mas apenas a analisa para encontrar a menor letra maiúscula presente, 
retornando-a como resultado. 
  *)

let transformacao formula =
  let letra_mais_pequena = ref 'Z' in
  let rec procura var =
    match var with
    | Var x ->
      for i = 0 to String.length x - 1 do
        let char = String.get x i in
        if char < !letra_mais_pequena && Char.code char >= 65 && Char.code char <= 90 then letra_mais_pequena := char
      done; !letra_mais_pequena
    | Not x -> procura x
    | And (x, y)-> min (procura x) (procura y)
    | Or (x, y) -> min (procura x) (procura y)
    | Implies (x, y) -> min (procura x) (procura y)
    | Equiv (x, y) -> min (procura x) (procura y)
    | _ -> !letra_mais_pequena
  in procura formula


(*

A função nts (simplificação de NOR to string) tem como objetivo fazer a tradução literal de formulas em lógica proposicional para formulas que usem exclusivamente o NOR.

- A função basicamente lê a string dada pelo utilizador e procura rápidamente entender a formula em lógica proposicional, usando a library F_parse fornecida pelos professores.
- Caso encontre a formula da string dada no F_parser, faz automaticamente a tradução para NOR's em string, usando a formatação pedida, isto é % como simbolo do NOR.
- Para casos mais complexos como o Implies e Equiv foi nos dito para chamar as formulas básicas (and, not, or) para executar de forma eficiente e correta
- Para o True e o False, usamos uma variável global que tem o char Z inicializada. O objetivo é que a variavel global seja alvo de alterações por parte da função transformação

Apesar de tudo, na primeira tentativa de implentação desta função, tentamos apenas fazer a conversão para nor, e após algumas tentativas falhadas, 
chegamos à conlcusão que seria melhor traduzir para uma string em vez de criar uma função de conversão de nor para string, chegando ao resultado seguinte:

*)

let rec nts teste = match teste with
| Var x -> x
| Not(Or (x,y)) -> "(" ^ nts x ^ " % " ^ nts y ^ ")"
| Not x -> "("^ nts x ^" % "^ nts x ^")"
| And (x,y) -> "(("^ nts x ^" % "^ nts x ^") % ("^ nts y ^" % "^ nts y ^"))"
| Or (x,y) -> "(("^ nts x ^ " % " ^ nts y ^") % (" ^ nts x ^ " % " ^ nts y ^"))"
| Implies (x,y) -> nts (Or (Not x, y))
| Equiv (x,y) -> nts (And(Implies(x,y), Implies(y,x)))
| False -> "(" ^ !var_global ^ " % (" ^ !var_global ^ " % " ^ !var_global ^ "))" 
| True ->   nts (Not False) 

(* A seguinte função funciona como uma especie de main
   - Começa por ler o input da função usando a função parse da library F_parser para ler a string em lógica proposicional
   - Após isso vai verificar se é necessário realizar alguma transformação para isso usa função escaped da library char do Ocaml, 
   que tem como objetivo retornar uma string representando o caracter fornecido, seguindo as convenções lexicais do OCaml. 
   - Se não precisar de transformação, procede para a conversão para formula NOR, para isso lê o primeiro elemento da lista, criada pelo F_Parse com a função hd 
   da library List
   -Por fim, caso não haja nenhum input por parte do utilizador, o programa fecha*)
let ()=
  let rec input = parse "stdin" in
  match input with
  | Some a ->
    var_global := Char.escaped (transformacao(List.hd a));
    let conversao_str = nts (List.hd a) in
    print_endline(conversao_str);
  | _ -> exit 0;;
  
 

  