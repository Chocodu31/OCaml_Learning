(* GRADE:  52% *)
(* let rec dernier l = match l with
   | [] -> failwith "denier"
   | [e] -> e
     | x::l -> dernier
*)

let rec hanoi (source, temp, dest) n =
  if n <= 0 then [] 
  else hanoi (source, dest, temp) (n-1) @ 
       (source, dest) :: hanoi (temp, source, dest) (n-1) 
         
         (* 2n-1 *)
                                                            
let rec map f l = 
  match l with
  | [] -> []
  | x :: l -> f x :: (map f) l 
  (* 
   1 fontion à plusieurs arguments (dite curryfiée)
   est 1 fonction qui prend le 1er argument
   et qui retourne une fonction qui s'occupe de lire le 2e argument.

   L'opération consistant à appliquer une fonction à n arguments
   à un seul argument (ou en tout cas, moins que n) s'appelle
   une application partielle. *)

                (* n + 1 *)

let rec inserer x l =
  match l with
  | [] -> x :: l
  | a :: l -> if x <= a then x :: a :: l else a :: inserer x l
                                                
let rec triInsertion l =
  match l with
  | [] -> []
  | [a] -> l
  | a :: l' -> inserer a (triInsertion l') 
                 
let rec partage l =
  match l with
  | [] -> [],[]
  | [a] -> [a] , []
  | a :: b :: l' -> let l1,l2 = partage l' in 
      a::l1,b::l2
  
let rec merge l1 l2 =
  match l1, l2 with
  | [], l2' -> l2'
  | l1', [] -> l1'
  | x :: l1', y :: l2' -> if x < y then x :: merge l1' l2
      else y :: merge l1 l2'
             
let rec triFusion l =
  match l with
  | [] -> []
  | [a] -> l
  | _ -> let l1, l2 = partage l in merge (triFusion l1) (triFusion l2)
