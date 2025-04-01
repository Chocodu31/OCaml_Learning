(* GRADE:  100% *)
let multiple_of n d = n mod d = 0
    
let integer_square_root n = int_of_float(sqrt(float_of_int(n))) 
    
let last_character s = String.get s (String.length(s)- 1)
                            
let string_of_bool b =
  if b then "true"
  else "false"
  
let pairwise_distinct (a,b,c,d) = a<>b 
                                  && a<>c 
                                  && a<>d 
                                  && b<>c 
                                  && b<>d 
                                  && c<>d 
                                     
let e1 = (1, true), 1
let e2 = (fun f -> f), 'a'
         
let f1 n b = (n*n, b && b)
let f2 a = true
  
let f3 a b = 
  let temp1 = a = b
  in 0
    
let f4 (a,b) = (b,a) 
               
let f5 fb fc a = fc a (fb a)
  
let f6 (fb,fc,a) = fc (a,fb a)
