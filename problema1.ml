(**
 * @file probA.ml
 * @brief A resposta é 42.
 * @details Este programa é um simples jogo. Após receber um valor do utilizador,
 vai aplicando regras a esse valor até o valor devolvido ser exatamente 42. Se não for possível,
 imprime 'BAD LUCK', caso contrário, imprime o menor número de passos usados para resolver o problema.
 * @author Luís Sá, a 46753
   @author Tiago Barreiros, a46118
 **)

(**
 * Variáveis Globais 'n' e 'x' usadas ao longo de todo o programa.
 * O valor guardado na variável n é o parâmetro fornecido na linha de comandos.
 **)
 let n = read_int() let x = ref 1000000

(**
 * Função Recursiva 'montante n', vai receber como valor de entrada o valor de 'n'.
 * De seguinda, faz várias validações:
   - se o valor for menor que 42, devolve o conteúdo da variável 'x',
   que neste caso é um valor estupidamente grande de passos;
   - se o valor for igual a 42, devolve exatamente zero, pois é o valor pretendido;
   - se nenhuma destas validações se confirmar, segue para as regras.
 **)
 let rec montante n =
 match n with
 | _ when n < 42 -> !x
 | _ when n = 42 -> 0
 | _ -> (
(**
 * Regras:
   - REGRA 1: caso o valor de 'n' seja divisível por dois, chama a função 'montante' com o seu valor
   de entrada reduzido a metade;
   - REGRA 2: caso o valor de 'n' seja divisível por três ou por quatro, e não terminar com zero, 
   pois qualquer número a múltiplicar por zero, resulta no próprio zero, chama a função 'montante', 
   mas desta vez retira ao valor 'n' a múltiplicação dos seus dois últimos dı́gitos;
   - REGRA 3: caso o valor de 'n' seja divisível por cinco, chama a função motante, onde retira a 'n'
   exatamente 42.
 **)
          let regra1 = 
            if (n mod 2 = 0) then montante (n/2) else !x in
          let regra2 = 
            if (n mod 3 = 0 || n mod 4 == 0) && (((n/10) mod 10) <> 0) then let res = ref 0 in 
          res := n mod 10; let res2 = ref 0 in 
          res2 := n mod 100; res2 := !res2 / 10; montante (n - (!res * !res2)) else !x in
          let regra3 = 
            if (n mod 5 = 0) then montante (n - 42) else !x in 
    
(**
  A função 'compara', como o próprio nome indica, vai comparar todas as regras e devolver o menor valor.
 **)
          let compara =
            if regra1 < regra2 && regra1 < regra3 then regra1 
            else if regra3 < regra2 && regra3 < regra1 then regra3 else regra2 in 
            compara + 1)

(** A função 'resultado', valida e imprime a conclusão.*)
   let resultado p = if p >= !x then Printf.printf"BAD LUCK\n" else Printf.printf"Passos dados %d\n" p

(** Execução Principal *)
   let () = if n > 0 && n < 1000000 then resultado (montante n) else raise (Invalid_argument "montante")