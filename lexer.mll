{ (* Header is just some arbitrary OCaml code *)
    (*Here may go some open directives, type declarations, defining aux functions*)
    (* TODO - if we encounter some error during lexing, there should be a 
     * helper method here that would report it 
     *)

     (* We will define tokens in the parser.mly and the menhir will automatically 
      * generate token type for us. *)
     open Parser
}

(* Then, here we might define some aux regular expressions.

let somerexpr1 = ...
let somerexpr2 = ...

examples may be 

let lol = "true" | "false"
let kek = ['0'-'9']+
...
*)

let lowercase_letter = ['a' - 'z']
let digit = ['0' - '9']
let ident = lowercase_letter (digit | lowercase_letter)* 

(* Here we go | <regex> {some ocaml code that should return something} 
 * this 'rule' syntax is kinda similar to the function in a sense, that we can 
 * call it. For example we could define helper rules like this: 
 * rule a = ... and b = ... and then use b within a. Those 'functions'  
 * take implicit argument called lexbuf
 *)
rule token = parse
|  

{ (* Footer is just some arbitrary OCaml code *)
}
