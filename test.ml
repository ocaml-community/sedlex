let x = 1
let regexp letter = [ 'a'-'z' ]
let regexp number = [ '0'-'9' ]

let rec token = lexer with
  | letter+ -> true
  | number+ -> false
 

    
