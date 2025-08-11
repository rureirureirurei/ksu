open Ast

(* Builtin primitive definitions as AST expressions *)
let builtin_definitions : top_expr list =
  let synthetic = Ast.synthetic in
  [
    (* Arithmetic primitives *)
    {
      value =
        Define
          {
            name = "+";
            expr =
              synthetic
                (Lambda
                   {
                     ids = [ "a0"; "a1" ];
                      body =
                        synthetic
                          (App
                             {
                               func = synthetic (Prim "+");
                               args =
                                 [
                                   synthetic (Var "a0");
                                   synthetic (Var "a1");
                                 ];
                             });
                   });
          };
      id = fresh_node_tag ();
      loc = { file = ""; line = 0; column = 0 };
    };
    {
      value =
        Define
          {
            name = "*";
            expr =
              synthetic
                (Lambda
                   {
                     ids = [ "a0"; "a1" ];
                      body =
                        synthetic
                          (App
                             {
                               func = synthetic (Prim "*");
                               args =
                                 [
                                   synthetic (Var "a0");
                                   synthetic (Var "a1");
                                 ];
                             });
                   });
          };
      id = fresh_node_tag ();
      loc = { file = ""; line = 0; column = 0 };
    };
    {
      value =
        Define
          {
            name = "-";
            expr =
              synthetic
                (Lambda
                   {
                     ids = [ "a0"; "a1" ];
                      body =
                        synthetic
                          (App
                             {
                               func = synthetic (Prim "-");
                               args =
                                 [
                                   synthetic (Var "a0");
                                   synthetic (Var "a1");
                                 ];
                             });
                   });
          };
      id = fresh_node_tag ();
      loc = { file = ""; line = 0; column = 0 };
    };
    {
      value =
        Define
          {
            name = "/";
            expr =
              synthetic
                (Lambda
                   {
                     ids = [ "a0"; "a1" ];
                      body =
                        synthetic
                          (App
                             {
                               func = synthetic (Prim "/");
                               args =
                                 [
                                   synthetic (Var "a0");
                                   synthetic (Var "a1");
                                 ];
                             });
                   });
          };
      id = fresh_node_tag ();
      loc = { file = ""; line = 0; column = 0 };
    };
    (* Comparison primitives *)
    {
      value =
        Define
          {
            name = "=";
            expr =
              synthetic
                (Lambda
                   {
                     ids = [ "a0"; "a1" ];
                      body =
                        synthetic
                          (App
                             {
                               func = synthetic (Prim "=");
                               args =
                                 [
                                   synthetic (Var "a0");
                                   synthetic (Var "a1");
                                 ];
                             });
                   });
          };
      id = fresh_node_tag ();
      loc = { file = ""; line = 0; column = 0 };
    };
    {
      value =
        Define
          {
            name = "<";
            expr =
              synthetic
                (Lambda
                   {
                     ids = [ "a0"; "a1" ];
                      body =
                        synthetic
                          (App
                             {
                               func = synthetic (Prim "<");
                               args =
                                 [
                                   synthetic (Var "a0");
                                   synthetic (Var "a1");
                                 ];
                             });
                   });
          };
      id = fresh_node_tag ();
      loc = { file = ""; line = 0; column = 0 };
    };
    {
      value =
        Define
          {
            name = ">";
            expr =
              synthetic
                (Lambda
                   {
                     ids = [ "a0"; "a1" ];
                      body =
                        synthetic
                          (App
                             {
                               func = synthetic (Prim ">");
                               args =
                                 [
                                   synthetic (Var "a0");
                                   synthetic (Var "a1");
                                 ];
                             });
                   });
          };
      id = fresh_node_tag ();
      loc = { file = ""; line = 0; column = 0 };
    };
    {
      value =
        Define
          {
            name = "<=";
            expr =
              synthetic
                (Lambda
                   {
                     ids = [ "a0"; "a1" ];
                      body =
                        synthetic
                          (App
                             {
                               func = synthetic (Prim "<=");
                               args =
                                 [
                                   synthetic (Var "a0");
                                   synthetic (Var "a1");
                                 ];
                             });
                   });
          };
      id = fresh_node_tag ();
      loc = { file = ""; line = 0; column = 0 };
    };
    {
      value =
        Define
          {
            name = ">=";
            expr =
              synthetic
                (Lambda
                   {
                     ids = [ "a0"; "a1" ];
                      body =
                        synthetic
                          (App
                             {
                               func = synthetic (Prim ">=");
                               args =
                                 [
                                   synthetic (Var "a0");
                                   synthetic (Var "a1");
                                 ];
                             });
                   });
          };
      id = fresh_node_tag ();
      loc = { file = ""; line = 0; column = 0 };
    };
    {
      value =
        Define
          {
            name = "cons";
            expr =
              synthetic
                (Lambda
                   {
                     ids = [ "a0"; "a1" ];
                      body =
                        synthetic
                          (App
                             {
                               func = synthetic (Prim "cons");
                               args =
                                 [
                                   synthetic (Var "a0");
                                   synthetic (Var "a1");
                                 ];
                             });
                   });
          };
      id = fresh_node_tag ();
      loc = { file = ""; line = 0; column = 0 };
    };
    (* Constants *)
    {
      value = Define { name = "nil"; expr = synthetic Nil };
      id = fresh_node_tag ();
      loc = { file = ""; line = 0; column = 0 };
    };
    (* Predicates *)
    {
      value =
        Define
          {
            name = "null?";
            expr =
              synthetic
                (Lambda
                   {
                     ids = [ "a0" ];
                      body =
                        synthetic
                          (App { func = synthetic (Prim "null?"); args = [ synthetic (Var "a0") ] });
                   });
          };
      id = fresh_node_tag ();
      loc = { file = ""; line = 0; column = 0 };
    };
    {
      value =
        Define
          {
            name = "pair?";
            expr =
              synthetic
                (Lambda
                   {
                     ids = [ "a0" ];
                      body =
                        synthetic
                          (App { func = synthetic (Prim "pair?"); args = [ synthetic (Var "a0") ] });
                   });
          };
      id = fresh_node_tag ();
      loc = { file = ""; line = 0; column = 0 };
    };
    {
      value =
        Define
          {
            name = "list?";
            expr =
              synthetic
                (Lambda
                   {
                     ids = [ "a0" ];
                      body =
                        synthetic
                          (App { func = synthetic (Prim "list?"); args = [ synthetic (Var "a0") ] });
                   });
          };
      id = fresh_node_tag ();
      loc = { file = ""; line = 0; column = 0 };
    };
    {
      value =
        Define
          {
            name = "eq?";
            expr =
              synthetic
                (Lambda
                   {
                     ids = [ "a0"; "a1" ];
                      body =
                        synthetic
                          (App
                             {
                               func = synthetic (Prim "eq?");
                               args =
                                 [
                                   synthetic (Var "a0");
                                   synthetic (Var "a1");
                                 ];
                             });
                   });
          };
      id = fresh_node_tag ();
      loc = { file = ""; line = 0; column = 0 };
    };
    {
      value =
        Define
          {
            name = "list-ref";
            expr =
              synthetic
                (Lambda
                   {
                     ids = [ "a0"; "a1" ];
                      body =
                        synthetic
                          (App
                             {
                               func = synthetic (Prim "list-ref");
                               args =
                                 [
                                   synthetic (Var "a0");
                                   synthetic (Var "a1");
                                 ];
                             });
                   });
          };
      id = fresh_node_tag ();
      loc = { file = ""; line = 0; column = 0 };
    };
  ]
