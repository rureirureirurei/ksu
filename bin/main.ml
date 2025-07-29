let x = Compiler_lib.Ast.Symbol "lol"
let main () = print_endline (Compiler_lib.Ast.string_of_expr x)

let () = main ()
