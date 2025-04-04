(* GRADE:  100% *)
let estZero_v1 n =
  match n with 0 -> "zero" 
                           
(* Match_failure //toplevel//:2:2*)


let estZero_v2 n =
  match n with 
  | 0 -> "zero"
  | _ -> "nonZero"
    
let voyelle n =
  match n with 
  | 'a' | 'e' | 'i' | 'o' | 'u' | 'y' -> true 
  | _ -> false
    
let rang n =
  match n with
  | "lundi" -> 1
  | "mardi" -> 2
  | "mercredi" -> 3
  | "jeudi" -> 4 
  | "vendredi" -> 5
  | "samedi" -> 6
  | "dimanche" -> 7
  | _ -> 0
    
let inf a b =
  match rang a, rang b with
  | 1,2 | 2,3 | 3,4 | 4,5 | 5,6 | 6,7 | 7,1 -> true
  | _ -> false
    
let jsem a =
  match a with 
  | 1 -> "lundi" 
  | 2 -> "mardi"
  | 3 -> "mercredi"
  | 4 -> "jeudi"
  | 5 -> "vendredi"
  | 6 -> "samedi"
  | 7 -> "dimanche"
  | _ -> "jour inconnu"

let jourSucc1 a =
  match a with
  | "lundi" -> "mardi"
  | "mardi" -> "mercredi"
  | "mercredi" -> "jeudi"
  | "jeudi" -> "vendredi"
  | "vendredi" -> "samedi"
  | "samedi" -> "dimanche"
  | "dimanche" -> "lundi"
  | _ -> "jour inconnu"
    
let jourSucc2 a = 
  let b = (rang a) in 
  if b = 0 then jsem 0
  else if b+1 > 7 then jsem(1) 
  else jsem(b+1)
  
let jourSucc3 a =
  let b = rang a in
  if b = 0 then jsem 0 else jsem ((b mod 7)+1)
  
let jourPred1 a =
  match a with
  | "lundi" -> "dimanche"
  | "dimanche" -> "samedi"
  | "samedi" -> "vendredi"
  | "vendredi" -> "jeudi"
  | "jeudi" -> "mercredi"
  | "mercredi" -> "mardi"
  | "mardi" -> "lundi"
  | _ -> "jour inconnu"
    
let jourPred2 a = 
  let b = (rang a) in 
  if b = 0 then "jour inconnu"
  else if b-1 < 1 then jsem(7) 
  else jsem(b-1)

let jourPred3 a =
  let b = rang a in
  if b = 0 then jsem 0 else jsem( ( ( (b mod 7) + 5) mod 7)  +1)
      
let bissextile a = ((a mod 4 = 0) && (a mod 100 <> 0)) || (a mod 400 = 0)
                                                          
let nbjour m a = 
  match m with
  | 1 | 3 | 5 | 7 | 8 | 10 | 12 -> 31
  | 4 | 6 | 9 | 11 -> 30
  | 2 -> if bissextile a then 29 else 28 
  | _ -> failwith "erreur"
    
    
    
