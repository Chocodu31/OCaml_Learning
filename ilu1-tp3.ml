(* GRADE:  100% *)
let premierCh n = 
  if n<0 then failwith "AH"
  else let rec temp n = if n < 10 then n 
         else temp (toutSaufDer n) 
    in temp n 

let toutSaufPrem n =
  if n<0 then failwith "AH" 
  else let rec temp n = if n<10 then 0 
         else let res = temp(toutSaufDer n) in res * 10 + dernierCh n
    in temp n
      
let rec estPalindrome n = 
  let n' = abs n in 
  (-10 < n && n < 10) ||
  (premierCh n' = dernierCh n'
   && estPalindrome( toutSaufDer ( toutSaufPrem n' ) ))
      
let rec nbOccs c n = 
  if n < 10 then 
    if c = n then 1 else 0
  else if c = dernierCh n then
    (nbOccs c (toutSaufDer n)) + 1 
  else (nbOccs c (toutSaufDer n))
       
let rec iterer n f x = 
  if n = 0 then x 
  else (iterer (n-1) (f) (f x))
       
let id x = x
  
  (* compose : ('a -> 'b) -> ('c -> 'a) -> 'c -> 'b *)
let compose fb fa c = fb(fa(c)) 

let rec iterer2 n f = 
  if n = 0 then id 
  else compose (iterer2 (n-1) (f)) f 
  
      (*  ( * ) = fun x -> x* *)
let pow a b = iterer(b) (( * ) a) 1
      
let rec itererBis f p x = 
  if p x then x 
  else (itererBis f p ( f(x) ) ) 
  
let rec qqsoit n p = 
  (n <= 0) || (p n) && (qqsoit (n-1) p) 
                       
let fastpow n e = 
  if e < 0 then failwith "fastpow"
  else let rec temp n e = if e = 0 then 1
         else if e = 1 then n
         else if e land 1 = 0 then temp (n*n) (e asr 1)
         else n * temp (n*n) (e asr 1) in temp n e
  
let ack (m,n) =
  if m < 0 || n < 0 then failwith "AH"
  else let rec temp m n = if m = 0 then n+1
         else if m > 0 && n = 0 then temp (m-1) 1
         else temp (m-1) (temp m (n-1)) in temp m n
