open Test_lib
open Report

exception NotImplemented of string

let collect_top_level code_ast =
  let open Parsetree in
  let open Learnocaml_report in
  let tbl = Hashtbl.create 10 in
  let rec findlet = function
    | [] -> ()
    | { pstr_desc = Pstr_value (is_rec, bds); _ } :: rest ->
      let rec findvar = function
        | [] -> findlet rest
        | { pvb_pat = { ppat_desc = Ppat_var { Location.txt; _ }; _ } ; pvb_expr; _ } :: rest ->
          Hashtbl.add tbl txt pvb_expr; findvar rest
        | _ :: rest -> findvar rest in
      findvar bds; findlet rest
    | _ :: rest -> findlet rest
  in findlet code_ast; tbl

(* each toplevel definition in the student code, associated by name to its AST *)
let top_level_bindings = collect_top_level code_ast

type ('e, 'a) result = Failed of 'e | Ok of 'a

(* Synthesizes a fake expression. *)
let mkexp pexp_desc =
  let open Parsetree in
  { pexp_desc
  ; pexp_attributes = []
  ; pexp_loc = Location.none
  ; pexp_loc_stack = []
  }

(* Constructs a located form of something by attaching a ghost location. *)
let located txt = let open Asttypes in { txt; loc = Location.none }

(* Constructs an expression consisting only of a simple identifier *)
let lident s = Parsetree.Pexp_ident (Longident.Lident s |> located) |> mkexp

let arg_list es = List.map (fun e -> (Asttypes.Nolabel, e)) es

(* Constructs an application that uses _only_ positional arguments.
   Labelled arguments cannot be expressed with this function. *)
let apply head args = Parsetree.Pexp_apply (head, arg_list args) |> mkexp

(* Constructs a constructor application. Note that `e` is an option to allow
   nullary constructors. Constructors with arity >1 are represented as
   containing a Pexp_tuple. *)
let constr s e = Parsetree.Pexp_construct (Longident.Lident s |> located, e) |> mkexp

let tuple exps = Parsetree.Pexp_tuple exps |> mkexp

let int_const n = Parsetree.(Pexp_constant (Pconst_integer (string_of_int n, None))) |> mkexp

let cons e1 e2 = constr "::" (Some (tuple [e1; e2]))
let nil = constr "[]" None

(* Conveniently constructs a list expression from a list of expressions,
   by chaining together applications of the :: constructor. *)
let list_sugar exps = List.fold_right cons exps nil

(* checks whether two options are equal according to a given function to check inside *)
let option_equals o1 o2 inside = match o1, o2 with
  | Some x1, Some x2 -> inside x1 x2
  | None, None -> true
  | _ -> false

let constant_equals c1 c2 =
  let open Parsetree in
  match c1, c2 with
  | Pconst_string (s1, _, suf1), Pconst_string (s2, _, suf2) ->
    s1 = s2 && suf1 = suf2
  (* the remaining cases don't have locations to ignore, so we can just use
     structural equality: *)
  | c1, c2 -> c1 = c2

(* Compares two expressions for equality, ignoring their locations and attributes,
   concentrating only on the _content_ of the expressions *)
let rec exp_equals e1 e2 =
  let open Parsetree in
  match e1.pexp_desc, e2.pexp_desc with
  | Pexp_ident i1, Pexp_ident i2 -> i1.txt = i2.txt
  | Pexp_construct (c1, o1), Pexp_construct (c2, o2) ->
    c1.txt = c2.txt && option_equals o1 o2 exp_equals
  | Pexp_apply (head1, args1), Pexp_apply (head2, args2) ->
    List.length args1 = List.length args2 &&
    exp_equals head1 head2 &&
    List.for_all
      (fun ((l1, e1), (l2, e2)) -> l1 = l2 && exp_equals e1 e2)
      (List.combine args1 args2)
  | Pexp_tuple es1, Pexp_tuple es2 when List.length es1 = List.length es2 ->
      List.combine es1 es2 |> List.for_all (fun (e1, e2) -> exp_equals e1 e2)
  | Pexp_constant c1, Pexp_constant c2 -> constant_equals c1 c2
  (*
  | Pexp_let (is_rec1, bindings, body), Pexp_let (_, _, _) -> raise NotImplemented "fuck3"
  | Pexp_function cases1, Pexp_function cases2 -> raise NotImplemented "fuck2"
  | Pexp_fun (label1, def_val1, pat1, body1), Pexp_fun (_, _, _, _) -> raise (NotImplemented "fuck1")
  *)
  | _ -> raise (NotImplemented "fuck")

let plus e1 e2 = apply (lident "+") [e1; e2]

let solution_step_2 =
  plus (int_const 1)
    (apply (lident "sum") [list_sugar [int_const 2; int_const 3]])
let student_step_2 = Hashtbl.find top_level_bindings "sum_step_2"

(* let x = raise NotImplemented *)

(* Collects the expressions defined as toplevel bindings in the student code
   named "<prefix>_step_i" with i starting at 1 and going until we can't find a
   student definition anymore. *)
let student_trace bindings prefix =
  let rec go i =
    let step_name = prefix ^ "_step_" ^ string_of_int i in
    match Hashtbl.find_opt bindings step_name with
    | Some e -> e :: go (i + 1)
    | None -> []
  in
  go 1

let rec check_trace i sol_trace student_trace =
  match sol_trace, student_trace with
  | sol_exp :: _, student_exp :: _ when not (exp_equals sol_exp student_exp) ->
      [ Message
        ( [ Text ("Step " ^ string_of_int i ^ " doesn't match.") ]
        , Failure )
      ]
  | _ :: sol_trace, _ :: student_trace ->
      Message ([ Text ("Step " ^ string_of_int i ^ " matches.") ], Success 1)
      :: check_trace (i + 1) sol_trace student_trace
  | _ :: _, [] ->
      [ Message ( [ Text ("There are more steps to find...") ], Failure ) ]
  | [], _ :: _ ->
      [ Message
        ( [ Text ("You wrote too many steps. " ^
                  "The last step was a value, so no more steps are possible.") ]
        , Failure )
      ]
  | [], [] -> []

let check_function_trace function_name sol_trace =
  student_trace top_level_bindings function_name |> check_trace 1 sol_trace

let sum_solution_trace =
  [ apply
      (lident "sum")
      [list_sugar [int_const 1; int_const 2; int_const 3]]
  ; plus (int_const 1)
    (apply (lident "sum") [list_sugar [int_const 2; int_const 3]])
  ; plus (int_const 1)
    (plus (int_const 2) (apply (lident "sum") [list_sugar [int_const 3]]))
  ; plus (int_const 1)
    (plus (int_const 2) (plus (int_const 3) (apply (lident "sum") [list_sugar []])))
  ; plus (int_const 1) (plus (int_const 2) (plus (int_const 3) (int_const 0)))
  ; plus (int_const 1) (plus (int_const 2) (int_const 3))
  ; plus (int_const 1) (int_const 5)
  ; int_const 6
  ]

let sum_tr_solution_trace =
  [ apply (lident "sum_tr") [int_const 0; list_sugar [int_const 1; int_const 2; int_const 3]]
  ; apply (lident "sum_tr") [plus (int_const 0) (int_const 1); list_sugar [int_const 2; int_const 3]]
  ; apply (lident "sum_tr") [plus (int_const 1) (int_const 2); list_sugar [int_const 3]]
  ; apply (lident "sum_tr") [plus (int_const 3) (int_const 3); list_sugar []]
  ; int_const 6
  ]

let () =
  set_result @@
  ast_sanity_check code_ast @@ fun () ->
    [ Section
      ( [ Text "Tracing"; Code "sum" ]
      , check_function_trace "sum" sum_solution_trace )
    ; Section
      ( [ Text "Tracing"; Code "sum_tr" ]
      , check_function_trace "sum_tr" sum_tr_solution_trace )
    ]
