let n = read_int()
let x = ref 100000000
let rec montante n =
if n < 42 then !x else if n = 42 then 0 else 
  let regra1 = if (n mod 2 = 0) then montante (n/2) else !x in
  let regra2 = if (n mod 3 = 0 || n mod 4 == 0) && (((n/10) mod 10) <> 0) then let res = ref 0 in res := n mod 10; let res2 = ref 0 in res2 := n mod 100; res2 := !res2 / 10; montante (n - (!res * !res2)) else !x in
  let regra3 = if (n mod 5 = 0) then montante (n - 42) else !x in 

  let compara = if regra1 < regra2 && regra1 < regra3 then regra1 else if regra3 < regra2 && regra3 < regra1 then regra3 else regra2 in compara + 1
  let resultado p = if p >= !x then Printf.printf"BAD LUCK\n" else Printf.printf"Passos dados %d\n" p

  let () = if n > 0 && n < 1000000 then resultado (montante n) else ()