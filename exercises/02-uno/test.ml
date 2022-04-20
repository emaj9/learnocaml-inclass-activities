
open Test_lib
open Report 

let () =
  set_result @@
  ast_sanity_check code_ast @@ fun () ->
    [ Section
      ( [ Text "Hi"; Code "this is code haha" ]
      , [Message 
      ( [ Text ("This is a text")], Success 1)] )
    ]