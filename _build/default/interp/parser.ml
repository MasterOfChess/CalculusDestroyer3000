
module MenhirBasics = struct
  
  exception Error
  
  let _eRR =
    fun _s ->
      raise Error
  
  type token = 
    | WITH
    | TIMES
    | TAN
    | SIN
    | SIMPL
    | RPAREN
    | REPR
    | POWER
    | PLUS
    | OF
    | MINUS
    | LPAREN
    | LN
    | INTGR
    | INT of (
# 5 "interp/parser.mly"
       (int)
# 29 "interp/parser.ml"
  )
    | ID of (
# 6 "interp/parser.mly"
       (string)
# 34 "interp/parser.ml"
  )
    | FACT
    | EXP
    | EXIT
    | EOF
    | DRVT
    | DIV
    | COS
    | ARCTAN
    | ARCSIN
    | ARCCOS
  
end

include MenhirBasics

# 1 "interp/parser.mly"
  
open Ast

# 55 "interp/parser.ml"

type ('s, 'r) _menhir_state = 
  | MenhirState00 : ('s, _menhir_box_prog) _menhir_state
    (** State 00.
        Stack shape : .
        Start symbol: prog. *)

  | MenhirState01 : (('s, _menhir_box_prog) _menhir_cell1_TAN, _menhir_box_prog) _menhir_state
    (** State 01.
        Stack shape : TAN.
        Start symbol: prog. *)

  | MenhirState02 : (('s, _menhir_box_prog) _menhir_cell1_SIN, _menhir_box_prog) _menhir_state
    (** State 02.
        Stack shape : SIN.
        Start symbol: prog. *)

  | MenhirState03 : (('s, _menhir_box_prog) _menhir_cell1_SIMPL, _menhir_box_prog) _menhir_state
    (** State 03.
        Stack shape : SIMPL.
        Start symbol: prog. *)

  | MenhirState04 : (('s, _menhir_box_prog) _menhir_cell1_REPR, _menhir_box_prog) _menhir_state
    (** State 04.
        Stack shape : REPR.
        Start symbol: prog. *)

  | MenhirState05 : (('s, _menhir_box_prog) _menhir_cell1_MINUS, _menhir_box_prog) _menhir_state
    (** State 05.
        Stack shape : MINUS.
        Start symbol: prog. *)

  | MenhirState06 : (('s, _menhir_box_prog) _menhir_cell1_LPAREN, _menhir_box_prog) _menhir_state
    (** State 06.
        Stack shape : LPAREN.
        Start symbol: prog. *)

  | MenhirState07 : (('s, _menhir_box_prog) _menhir_cell1_LN, _menhir_box_prog) _menhir_state
    (** State 07.
        Stack shape : LN.
        Start symbol: prog. *)

  | MenhirState09 : (('s, _menhir_box_prog) _menhir_cell1_INTGR, _menhir_box_prog) _menhir_state
    (** State 09.
        Stack shape : INTGR.
        Start symbol: prog. *)

  | MenhirState12 : (('s, _menhir_box_prog) _menhir_cell1_EXP, _menhir_box_prog) _menhir_state
    (** State 12.
        Stack shape : EXP.
        Start symbol: prog. *)

  | MenhirState15 : (('s, _menhir_box_prog) _menhir_cell1_DRVT, _menhir_box_prog) _menhir_state
    (** State 15.
        Stack shape : DRVT.
        Start symbol: prog. *)

  | MenhirState16 : (('s, _menhir_box_prog) _menhir_cell1_COS, _menhir_box_prog) _menhir_state
    (** State 16.
        Stack shape : COS.
        Start symbol: prog. *)

  | MenhirState17 : (('s, _menhir_box_prog) _menhir_cell1_ARCTAN, _menhir_box_prog) _menhir_state
    (** State 17.
        Stack shape : ARCTAN.
        Start symbol: prog. *)

  | MenhirState18 : (('s, _menhir_box_prog) _menhir_cell1_ARCSIN, _menhir_box_prog) _menhir_state
    (** State 18.
        Stack shape : ARCSIN.
        Start symbol: prog. *)

  | MenhirState19 : (('s, _menhir_box_prog) _menhir_cell1_ARCCOS, _menhir_box_prog) _menhir_state
    (** State 19.
        Stack shape : ARCCOS.
        Start symbol: prog. *)

  | MenhirState21 : (('s, _menhir_box_prog) _menhir_cell1_expr, _menhir_box_prog) _menhir_state
    (** State 21.
        Stack shape : expr.
        Start symbol: prog. *)

  | MenhirState28 : (('s, _menhir_box_prog) _menhir_cell1_expr, _menhir_box_prog) _menhir_state
    (** State 28.
        Stack shape : expr.
        Start symbol: prog. *)

  | MenhirState30 : (('s, _menhir_box_prog) _menhir_cell1_expr, _menhir_box_prog) _menhir_state
    (** State 30.
        Stack shape : expr.
        Start symbol: prog. *)

  | MenhirState32 : (('s, _menhir_box_prog) _menhir_cell1_expr, _menhir_box_prog) _menhir_state
    (** State 32.
        Stack shape : expr.
        Start symbol: prog. *)

  | MenhirState34 : (('s, _menhir_box_prog) _menhir_cell1_expr, _menhir_box_prog) _menhir_state
    (** State 34.
        Stack shape : expr.
        Start symbol: prog. *)

  | MenhirState36 : ((('s, _menhir_box_prog) _menhir_cell1_DRVT, _menhir_box_prog) _menhir_cell1_expr, _menhir_box_prog) _menhir_state
    (** State 36.
        Stack shape : DRVT expr.
        Start symbol: prog. *)

  | MenhirState40 : ((('s, _menhir_box_prog) _menhir_cell1_INTGR, _menhir_box_prog) _menhir_cell1_expr, _menhir_box_prog) _menhir_state
    (** State 40.
        Stack shape : INTGR expr.
        Start symbol: prog. *)


and ('s, 'r) _menhir_cell1_expr = 
  | MenhirCell1_expr of 's * ('s, 'r) _menhir_state * (Ast.expr)

and ('s, 'r) _menhir_cell1_ARCCOS = 
  | MenhirCell1_ARCCOS of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_ARCSIN = 
  | MenhirCell1_ARCSIN of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_ARCTAN = 
  | MenhirCell1_ARCTAN of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_COS = 
  | MenhirCell1_COS of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_DRVT = 
  | MenhirCell1_DRVT of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_EXP = 
  | MenhirCell1_EXP of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_INTGR = 
  | MenhirCell1_INTGR of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_LN = 
  | MenhirCell1_LN of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_LPAREN = 
  | MenhirCell1_LPAREN of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_MINUS = 
  | MenhirCell1_MINUS of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_REPR = 
  | MenhirCell1_REPR of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_SIMPL = 
  | MenhirCell1_SIMPL of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_SIN = 
  | MenhirCell1_SIN of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_TAN = 
  | MenhirCell1_TAN of 's * ('s, 'r) _menhir_state

and _menhir_box_prog = 
  | MenhirBox_prog of (Ast.expr) [@@unboxed]

let _menhir_action_01 =
  fun i ->
    (
# 62 "interp/parser.mly"
           ( Int i )
# 222 "interp/parser.ml"
     : (Ast.expr))

let _menhir_action_02 =
  fun x ->
    (
# 63 "interp/parser.mly"
          ( Var x )
# 230 "interp/parser.ml"
     : (Ast.expr))

let _menhir_action_03 =
  fun e ->
    (
# 64 "interp/parser.mly"
                  ( Fact e )
# 238 "interp/parser.ml"
     : (Ast.expr))

let _menhir_action_04 =
  fun e1 e2 ->
    (
# 65 "interp/parser.mly"
                               ( Binop (Pow, e1, e2) )
# 246 "interp/parser.ml"
     : (Ast.expr))

let _menhir_action_05 =
  fun e ->
    (
# 66 "interp/parser.mly"
                 ( Fun ("ln", e) )
# 254 "interp/parser.ml"
     : (Ast.expr))

let _menhir_action_06 =
  fun e ->
    (
# 67 "interp/parser.mly"
                  ( Fun ("exp", e) )
# 262 "interp/parser.ml"
     : (Ast.expr))

let _menhir_action_07 =
  fun e ->
    (
# 68 "interp/parser.mly"
                  ( Fun ("sin", e) )
# 270 "interp/parser.ml"
     : (Ast.expr))

let _menhir_action_08 =
  fun e ->
    (
# 69 "interp/parser.mly"
                  ( Fun ("cos", e) )
# 278 "interp/parser.ml"
     : (Ast.expr))

let _menhir_action_09 =
  fun e ->
    (
# 70 "interp/parser.mly"
                  ( Fun ("tan", e) )
# 286 "interp/parser.ml"
     : (Ast.expr))

let _menhir_action_10 =
  fun e ->
    (
# 71 "interp/parser.mly"
                     ( Fun ("arcsin", e) )
# 294 "interp/parser.ml"
     : (Ast.expr))

let _menhir_action_11 =
  fun e ->
    (
# 72 "interp/parser.mly"
                     ( Fun ("arccos", e) )
# 302 "interp/parser.ml"
     : (Ast.expr))

let _menhir_action_12 =
  fun e ->
    (
# 73 "interp/parser.mly"
                     ( Fun ("arctan", e) )
# 310 "interp/parser.ml"
     : (Ast.expr))

let _menhir_action_13 =
  fun e1 e2 ->
    (
# 74 "interp/parser.mly"
                             ( Binop (Div, e1, e2) )
# 318 "interp/parser.ml"
     : (Ast.expr))

let _menhir_action_14 =
  fun e1 e2 ->
    (
# 75 "interp/parser.mly"
                               ( Binop (Mult, e1, e2) )
# 326 "interp/parser.ml"
     : (Ast.expr))

let _menhir_action_15 =
  fun e1 e2 ->
    (
# 76 "interp/parser.mly"
                               ( Binop (Sub, e1, e2) )
# 334 "interp/parser.ml"
     : (Ast.expr))

let _menhir_action_16 =
  fun e1 e2 ->
    (
# 77 "interp/parser.mly"
                              ( Binop (Add, e1, e2) )
# 342 "interp/parser.ml"
     : (Ast.expr))

let _menhir_action_17 =
  fun e ->
    (
# 78 "interp/parser.mly"
                    ( Neg e )
# 350 "interp/parser.ml"
     : (Ast.expr))

let _menhir_action_18 =
  fun e1 e2 ->
    (
# 79 "interp/parser.mly"
                                         ( Com (Derive(e1, e2)) )
# 358 "interp/parser.ml"
     : (Ast.expr))

let _menhir_action_19 =
  fun e1 e2 ->
    (
# 80 "interp/parser.mly"
                                          ( Com (Integrate(e1, e2)) )
# 366 "interp/parser.ml"
     : (Ast.expr))

let _menhir_action_20 =
  fun e ->
    (
# 81 "interp/parser.mly"
                    ( Com (Simpl e) )
# 374 "interp/parser.ml"
     : (Ast.expr))

let _menhir_action_21 =
  fun e ->
    (
# 82 "interp/parser.mly"
                   ( Com (Repr e) )
# 382 "interp/parser.ml"
     : (Ast.expr))

let _menhir_action_22 =
  fun () ->
    (
# 83 "interp/parser.mly"
         ( Com(Ext) )
# 390 "interp/parser.ml"
     : (Ast.expr))

let _menhir_action_23 =
  fun e ->
    (
# 84 "interp/parser.mly"
                            (e)
# 398 "interp/parser.ml"
     : (Ast.expr))

let _menhir_action_24 =
  fun e ->
    (
# 58 "interp/parser.mly"
                 ( e )
# 406 "interp/parser.ml"
     : (Ast.expr))

let _menhir_print_token : token -> string =
  fun _tok ->
    match _tok with
    | ARCCOS ->
        "ARCCOS"
    | ARCSIN ->
        "ARCSIN"
    | ARCTAN ->
        "ARCTAN"
    | COS ->
        "COS"
    | DIV ->
        "DIV"
    | DRVT ->
        "DRVT"
    | EOF ->
        "EOF"
    | EXIT ->
        "EXIT"
    | EXP ->
        "EXP"
    | FACT ->
        "FACT"
    | ID _ ->
        "ID"
    | INT _ ->
        "INT"
    | INTGR ->
        "INTGR"
    | LN ->
        "LN"
    | LPAREN ->
        "LPAREN"
    | MINUS ->
        "MINUS"
    | OF ->
        "OF"
    | PLUS ->
        "PLUS"
    | POWER ->
        "POWER"
    | REPR ->
        "REPR"
    | RPAREN ->
        "RPAREN"
    | SIMPL ->
        "SIMPL"
    | SIN ->
        "SIN"
    | TAN ->
        "TAN"
    | TIMES ->
        "TIMES"
    | WITH ->
        "WITH"

let _menhir_fail : unit -> 'a =
  fun () ->
    Printf.eprintf "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

include struct
  
  [@@@ocaml.warning "-4-37-39"]
  
  let rec _menhir_run_01 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_prog) _menhir_state -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_TAN (_menhir_stack, _menhir_s) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | TAN ->
          _menhir_run_01 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState01
      | SIN ->
          _menhir_run_02 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState01
      | SIMPL ->
          _menhir_run_03 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState01
      | REPR ->
          _menhir_run_04 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState01
      | MINUS ->
          _menhir_run_05 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState01
      | LPAREN ->
          _menhir_run_06 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState01
      | LN ->
          _menhir_run_07 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState01
      | INTGR ->
          _menhir_run_08 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState01
      | INT _v ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let i = _v in
          let _v = _menhir_action_01 i in
          _menhir_run_49 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState01 _tok
      | ID _v ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let x = _v in
          let _v = _menhir_action_02 x in
          _menhir_run_49 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState01 _tok
      | EXP ->
          _menhir_run_12 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState01
      | EXIT ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_22 () in
          _menhir_run_49 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState01 _tok
      | DRVT ->
          _menhir_run_14 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState01
      | COS ->
          _menhir_run_16 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState01
      | ARCTAN ->
          _menhir_run_17 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState01
      | ARCSIN ->
          _menhir_run_18 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState01
      | ARCCOS ->
          _menhir_run_19 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState01
      | _ ->
          _eRR ()
  
  and _menhir_run_02 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_prog) _menhir_state -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_SIN (_menhir_stack, _menhir_s) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | TAN ->
          _menhir_run_01 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState02
      | SIN ->
          _menhir_run_02 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState02
      | SIMPL ->
          _menhir_run_03 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState02
      | REPR ->
          _menhir_run_04 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState02
      | MINUS ->
          _menhir_run_05 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState02
      | LPAREN ->
          _menhir_run_06 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState02
      | LN ->
          _menhir_run_07 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState02
      | INTGR ->
          _menhir_run_08 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState02
      | INT _v ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let i = _v in
          let _v = _menhir_action_01 i in
          _menhir_run_48 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState02 _tok
      | ID _v ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let x = _v in
          let _v = _menhir_action_02 x in
          _menhir_run_48 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState02 _tok
      | EXP ->
          _menhir_run_12 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState02
      | EXIT ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_22 () in
          _menhir_run_48 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState02 _tok
      | DRVT ->
          _menhir_run_14 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState02
      | COS ->
          _menhir_run_16 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState02
      | ARCTAN ->
          _menhir_run_17 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState02
      | ARCSIN ->
          _menhir_run_18 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState02
      | ARCCOS ->
          _menhir_run_19 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState02
      | _ ->
          _eRR ()
  
  and _menhir_run_03 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_prog) _menhir_state -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_SIMPL (_menhir_stack, _menhir_s) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | TAN ->
          _menhir_run_01 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState03
      | SIN ->
          _menhir_run_02 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState03
      | SIMPL ->
          _menhir_run_03 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState03
      | REPR ->
          _menhir_run_04 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState03
      | MINUS ->
          _menhir_run_05 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState03
      | LPAREN ->
          _menhir_run_06 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState03
      | LN ->
          _menhir_run_07 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState03
      | INTGR ->
          _menhir_run_08 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState03
      | INT _v ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let i = _v in
          let _v = _menhir_action_01 i in
          _menhir_run_47 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState03 _tok
      | ID _v ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let x = _v in
          let _v = _menhir_action_02 x in
          _menhir_run_47 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState03 _tok
      | EXP ->
          _menhir_run_12 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState03
      | EXIT ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_22 () in
          _menhir_run_47 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState03 _tok
      | DRVT ->
          _menhir_run_14 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState03
      | COS ->
          _menhir_run_16 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState03
      | ARCTAN ->
          _menhir_run_17 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState03
      | ARCSIN ->
          _menhir_run_18 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState03
      | ARCCOS ->
          _menhir_run_19 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState03
      | _ ->
          _eRR ()
  
  and _menhir_run_04 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_prog) _menhir_state -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_REPR (_menhir_stack, _menhir_s) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | TAN ->
          _menhir_run_01 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState04
      | SIN ->
          _menhir_run_02 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState04
      | SIMPL ->
          _menhir_run_03 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState04
      | REPR ->
          _menhir_run_04 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState04
      | MINUS ->
          _menhir_run_05 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState04
      | LPAREN ->
          _menhir_run_06 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState04
      | LN ->
          _menhir_run_07 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState04
      | INTGR ->
          _menhir_run_08 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState04
      | INT _v ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let i = _v in
          let _v = _menhir_action_01 i in
          _menhir_run_46 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState04 _tok
      | ID _v ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let x = _v in
          let _v = _menhir_action_02 x in
          _menhir_run_46 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState04 _tok
      | EXP ->
          _menhir_run_12 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState04
      | EXIT ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_22 () in
          _menhir_run_46 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState04 _tok
      | DRVT ->
          _menhir_run_14 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState04
      | COS ->
          _menhir_run_16 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState04
      | ARCTAN ->
          _menhir_run_17 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState04
      | ARCSIN ->
          _menhir_run_18 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState04
      | ARCCOS ->
          _menhir_run_19 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState04
      | _ ->
          _eRR ()
  
  and _menhir_run_05 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_prog) _menhir_state -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_MINUS (_menhir_stack, _menhir_s) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | TAN ->
          _menhir_run_01 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState05
      | SIN ->
          _menhir_run_02 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState05
      | SIMPL ->
          _menhir_run_03 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState05
      | REPR ->
          _menhir_run_04 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState05
      | MINUS ->
          _menhir_run_05 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState05
      | LPAREN ->
          _menhir_run_06 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState05
      | LN ->
          _menhir_run_07 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState05
      | INTGR ->
          _menhir_run_08 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState05
      | INT _v ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let i = _v in
          let _v = _menhir_action_01 i in
          _menhir_run_45 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState05 _tok
      | ID _v ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let x = _v in
          let _v = _menhir_action_02 x in
          _menhir_run_45 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState05 _tok
      | EXP ->
          _menhir_run_12 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState05
      | EXIT ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_22 () in
          _menhir_run_45 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState05 _tok
      | DRVT ->
          _menhir_run_14 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState05
      | COS ->
          _menhir_run_16 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState05
      | ARCTAN ->
          _menhir_run_17 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState05
      | ARCSIN ->
          _menhir_run_18 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState05
      | ARCCOS ->
          _menhir_run_19 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState05
      | _ ->
          _eRR ()
  
  and _menhir_run_06 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_prog) _menhir_state -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_LPAREN (_menhir_stack, _menhir_s) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | TAN ->
          _menhir_run_01 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState06
      | SIN ->
          _menhir_run_02 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState06
      | SIMPL ->
          _menhir_run_03 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState06
      | REPR ->
          _menhir_run_04 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState06
      | MINUS ->
          _menhir_run_05 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState06
      | LPAREN ->
          _menhir_run_06 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState06
      | LN ->
          _menhir_run_07 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState06
      | INTGR ->
          _menhir_run_08 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState06
      | INT _v ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let i = _v in
          let _v = _menhir_action_01 i in
          _menhir_run_43 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState06 _tok
      | ID _v ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let x = _v in
          let _v = _menhir_action_02 x in
          _menhir_run_43 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState06 _tok
      | EXP ->
          _menhir_run_12 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState06
      | EXIT ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_22 () in
          _menhir_run_43 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState06 _tok
      | DRVT ->
          _menhir_run_14 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState06
      | COS ->
          _menhir_run_16 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState06
      | ARCTAN ->
          _menhir_run_17 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState06
      | ARCSIN ->
          _menhir_run_18 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState06
      | ARCCOS ->
          _menhir_run_19 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState06
      | _ ->
          _eRR ()
  
  and _menhir_run_07 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_prog) _menhir_state -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_LN (_menhir_stack, _menhir_s) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | TAN ->
          _menhir_run_01 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState07
      | SIN ->
          _menhir_run_02 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState07
      | SIMPL ->
          _menhir_run_03 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState07
      | REPR ->
          _menhir_run_04 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState07
      | MINUS ->
          _menhir_run_05 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState07
      | LPAREN ->
          _menhir_run_06 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState07
      | LN ->
          _menhir_run_07 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState07
      | INTGR ->
          _menhir_run_08 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState07
      | INT _v ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let i = _v in
          let _v = _menhir_action_01 i in
          _menhir_run_42 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState07 _tok
      | ID _v ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let x = _v in
          let _v = _menhir_action_02 x in
          _menhir_run_42 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState07 _tok
      | EXP ->
          _menhir_run_12 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState07
      | EXIT ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_22 () in
          _menhir_run_42 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState07 _tok
      | DRVT ->
          _menhir_run_14 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState07
      | COS ->
          _menhir_run_16 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState07
      | ARCTAN ->
          _menhir_run_17 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState07
      | ARCSIN ->
          _menhir_run_18 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState07
      | ARCCOS ->
          _menhir_run_19 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState07
      | _ ->
          _eRR ()
  
  and _menhir_run_08 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_prog) _menhir_state -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_INTGR (_menhir_stack, _menhir_s) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | WITH ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | TAN ->
              _menhir_run_01 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState09
          | SIN ->
              _menhir_run_02 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState09
          | SIMPL ->
              _menhir_run_03 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState09
          | REPR ->
              _menhir_run_04 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState09
          | MINUS ->
              _menhir_run_05 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState09
          | LPAREN ->
              _menhir_run_06 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState09
          | LN ->
              _menhir_run_07 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState09
          | INTGR ->
              _menhir_run_08 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState09
          | INT _v ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let i = _v in
              let _v = _menhir_action_01 i in
              _menhir_run_39 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState09 _tok
          | ID _v ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let x = _v in
              let _v = _menhir_action_02 x in
              _menhir_run_39 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState09 _tok
          | EXP ->
              _menhir_run_12 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState09
          | EXIT ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _v = _menhir_action_22 () in
              _menhir_run_39 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState09 _tok
          | DRVT ->
              _menhir_run_14 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState09
          | COS ->
              _menhir_run_16 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState09
          | ARCTAN ->
              _menhir_run_17 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState09
          | ARCSIN ->
              _menhir_run_18 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState09
          | ARCCOS ->
              _menhir_run_19 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState09
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_39 : type  ttv_stack. ((ttv_stack, _menhir_box_prog) _menhir_cell1_INTGR as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_prog) _menhir_state -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | TIMES ->
          _menhir_run_28 _menhir_stack _menhir_lexbuf _menhir_lexer
      | POWER ->
          _menhir_run_21 _menhir_stack _menhir_lexbuf _menhir_lexer
      | PLUS ->
          _menhir_run_32 _menhir_stack _menhir_lexbuf _menhir_lexer
      | OF ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | TAN ->
              _menhir_run_01 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState40
          | SIN ->
              _menhir_run_02 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState40
          | SIMPL ->
              _menhir_run_03 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState40
          | REPR ->
              _menhir_run_04 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState40
          | MINUS ->
              _menhir_run_05 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState40
          | LPAREN ->
              _menhir_run_06 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState40
          | LN ->
              _menhir_run_07 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState40
          | INTGR ->
              _menhir_run_08 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState40
          | INT _v_0 ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let i = _v_0 in
              let _v = _menhir_action_01 i in
              _menhir_run_41 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState40 _tok
          | ID _v_2 ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let x = _v_2 in
              let _v = _menhir_action_02 x in
              _menhir_run_41 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState40 _tok
          | EXP ->
              _menhir_run_12 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState40
          | EXIT ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _v = _menhir_action_22 () in
              _menhir_run_41 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState40 _tok
          | DRVT ->
              _menhir_run_14 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState40
          | COS ->
              _menhir_run_16 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState40
          | ARCTAN ->
              _menhir_run_17 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState40
          | ARCSIN ->
              _menhir_run_18 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState40
          | ARCCOS ->
              _menhir_run_19 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState40
          | _ ->
              _eRR ())
      | MINUS ->
          _menhir_run_34 _menhir_stack _menhir_lexbuf _menhir_lexer
      | FACT ->
          _menhir_run_23 _menhir_stack _menhir_lexbuf _menhir_lexer
      | DIV ->
          _menhir_run_30 _menhir_stack _menhir_lexbuf _menhir_lexer
      | _ ->
          _eRR ()
  
  and _menhir_run_28 : type  ttv_stack. (ttv_stack, _menhir_box_prog) _menhir_cell1_expr -> _ -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | TAN ->
          _menhir_run_01 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState28
      | SIN ->
          _menhir_run_02 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState28
      | SIMPL ->
          _menhir_run_03 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState28
      | REPR ->
          _menhir_run_04 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState28
      | MINUS ->
          _menhir_run_05 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState28
      | LPAREN ->
          _menhir_run_06 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState28
      | LN ->
          _menhir_run_07 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState28
      | INTGR ->
          _menhir_run_08 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState28
      | INT _v ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let i = _v in
          let _v = _menhir_action_01 i in
          _menhir_run_29 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState28 _tok
      | ID _v ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let x = _v in
          let _v = _menhir_action_02 x in
          _menhir_run_29 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState28 _tok
      | EXP ->
          _menhir_run_12 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState28
      | EXIT ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_22 () in
          _menhir_run_29 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState28 _tok
      | DRVT ->
          _menhir_run_14 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState28
      | COS ->
          _menhir_run_16 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState28
      | ARCTAN ->
          _menhir_run_17 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState28
      | ARCSIN ->
          _menhir_run_18 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState28
      | ARCCOS ->
          _menhir_run_19 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState28
      | _ ->
          _eRR ()
  
  and _menhir_run_29 : type  ttv_stack. ((ttv_stack, _menhir_box_prog) _menhir_cell1_expr as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_prog) _menhir_state -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | POWER ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_21 _menhir_stack _menhir_lexbuf _menhir_lexer
      | FACT ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_23 _menhir_stack _menhir_lexbuf _menhir_lexer
      | DIV ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_30 _menhir_stack _menhir_lexbuf _menhir_lexer
      | EOF | MINUS | OF | PLUS | RPAREN | TIMES ->
          let MenhirCell1_expr (_menhir_stack, _menhir_s, e1) = _menhir_stack in
          let e2 = _v in
          let _v = _menhir_action_14 e1 e2 in
          _menhir_goto_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_21 : type  ttv_stack. (ttv_stack, _menhir_box_prog) _menhir_cell1_expr -> _ -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | TAN ->
          _menhir_run_01 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState21
      | SIN ->
          _menhir_run_02 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState21
      | SIMPL ->
          _menhir_run_03 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState21
      | REPR ->
          _menhir_run_04 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState21
      | MINUS ->
          _menhir_run_05 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState21
      | LPAREN ->
          _menhir_run_06 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState21
      | LN ->
          _menhir_run_07 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState21
      | INTGR ->
          _menhir_run_08 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState21
      | INT _v ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let i = _v in
          let _v = _menhir_action_01 i in
          _menhir_run_22 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState21 _tok
      | ID _v ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let x = _v in
          let _v = _menhir_action_02 x in
          _menhir_run_22 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState21 _tok
      | EXP ->
          _menhir_run_12 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState21
      | EXIT ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_22 () in
          _menhir_run_22 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState21 _tok
      | DRVT ->
          _menhir_run_14 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState21
      | COS ->
          _menhir_run_16 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState21
      | ARCTAN ->
          _menhir_run_17 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState21
      | ARCSIN ->
          _menhir_run_18 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState21
      | ARCCOS ->
          _menhir_run_19 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState21
      | _ ->
          _eRR ()
  
  and _menhir_run_22 : type  ttv_stack. ((ttv_stack, _menhir_box_prog) _menhir_cell1_expr as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_prog) _menhir_state -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | POWER ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_21 _menhir_stack _menhir_lexbuf _menhir_lexer
      | FACT ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_23 _menhir_stack _menhir_lexbuf _menhir_lexer
      | DIV | EOF | MINUS | OF | PLUS | RPAREN | TIMES ->
          let MenhirCell1_expr (_menhir_stack, _menhir_s, e1) = _menhir_stack in
          let e2 = _v in
          let _v = _menhir_action_04 e1 e2 in
          _menhir_goto_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_23 : type  ttv_stack. (ttv_stack, _menhir_box_prog) _menhir_cell1_expr -> _ -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let MenhirCell1_expr (_menhir_stack, _menhir_s, e) = _menhir_stack in
      let _v = _menhir_action_03 e in
      _menhir_goto_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_expr : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_prog) _menhir_state -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState00 ->
          _menhir_run_51 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState01 ->
          _menhir_run_49 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState02 ->
          _menhir_run_48 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState03 ->
          _menhir_run_47 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState04 ->
          _menhir_run_46 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState05 ->
          _menhir_run_45 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState06 ->
          _menhir_run_43 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState07 ->
          _menhir_run_42 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState40 ->
          _menhir_run_41 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState09 ->
          _menhir_run_39 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState12 ->
          _menhir_run_38 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState36 ->
          _menhir_run_37 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState34 ->
          _menhir_run_35 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState32 ->
          _menhir_run_33 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState30 ->
          _menhir_run_31 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState28 ->
          _menhir_run_29 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState15 ->
          _menhir_run_27 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState16 ->
          _menhir_run_26 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState17 ->
          _menhir_run_25 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState18 ->
          _menhir_run_24 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState21 ->
          _menhir_run_22 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState19 ->
          _menhir_run_20 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_51 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_prog) _menhir_state -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | TIMES ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_28 _menhir_stack _menhir_lexbuf _menhir_lexer
      | POWER ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_21 _menhir_stack _menhir_lexbuf _menhir_lexer
      | PLUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_32 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MINUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_34 _menhir_stack _menhir_lexbuf _menhir_lexer
      | FACT ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_23 _menhir_stack _menhir_lexbuf _menhir_lexer
      | EOF ->
          let e = _v in
          let _v = _menhir_action_24 e in
          MenhirBox_prog _v
      | DIV ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_30 _menhir_stack _menhir_lexbuf _menhir_lexer
      | _ ->
          _eRR ()
  
  and _menhir_run_32 : type  ttv_stack. (ttv_stack, _menhir_box_prog) _menhir_cell1_expr -> _ -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | TAN ->
          _menhir_run_01 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState32
      | SIN ->
          _menhir_run_02 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState32
      | SIMPL ->
          _menhir_run_03 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState32
      | REPR ->
          _menhir_run_04 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState32
      | MINUS ->
          _menhir_run_05 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState32
      | LPAREN ->
          _menhir_run_06 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState32
      | LN ->
          _menhir_run_07 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState32
      | INTGR ->
          _menhir_run_08 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState32
      | INT _v ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let i = _v in
          let _v = _menhir_action_01 i in
          _menhir_run_33 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState32 _tok
      | ID _v ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let x = _v in
          let _v = _menhir_action_02 x in
          _menhir_run_33 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState32 _tok
      | EXP ->
          _menhir_run_12 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState32
      | EXIT ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_22 () in
          _menhir_run_33 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState32 _tok
      | DRVT ->
          _menhir_run_14 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState32
      | COS ->
          _menhir_run_16 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState32
      | ARCTAN ->
          _menhir_run_17 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState32
      | ARCSIN ->
          _menhir_run_18 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState32
      | ARCCOS ->
          _menhir_run_19 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState32
      | _ ->
          _eRR ()
  
  and _menhir_run_33 : type  ttv_stack. ((ttv_stack, _menhir_box_prog) _menhir_cell1_expr as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_prog) _menhir_state -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | TIMES ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_28 _menhir_stack _menhir_lexbuf _menhir_lexer
      | POWER ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_21 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MINUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_34 _menhir_stack _menhir_lexbuf _menhir_lexer
      | FACT ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_23 _menhir_stack _menhir_lexbuf _menhir_lexer
      | DIV ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_30 _menhir_stack _menhir_lexbuf _menhir_lexer
      | EOF | OF | PLUS | RPAREN ->
          let MenhirCell1_expr (_menhir_stack, _menhir_s, e1) = _menhir_stack in
          let e2 = _v in
          let _v = _menhir_action_16 e1 e2 in
          _menhir_goto_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_34 : type  ttv_stack. (ttv_stack, _menhir_box_prog) _menhir_cell1_expr -> _ -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | TAN ->
          _menhir_run_01 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState34
      | SIN ->
          _menhir_run_02 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState34
      | SIMPL ->
          _menhir_run_03 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState34
      | REPR ->
          _menhir_run_04 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState34
      | MINUS ->
          _menhir_run_05 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState34
      | LPAREN ->
          _menhir_run_06 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState34
      | LN ->
          _menhir_run_07 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState34
      | INTGR ->
          _menhir_run_08 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState34
      | INT _v ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let i = _v in
          let _v = _menhir_action_01 i in
          _menhir_run_35 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState34 _tok
      | ID _v ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let x = _v in
          let _v = _menhir_action_02 x in
          _menhir_run_35 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState34 _tok
      | EXP ->
          _menhir_run_12 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState34
      | EXIT ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_22 () in
          _menhir_run_35 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState34 _tok
      | DRVT ->
          _menhir_run_14 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState34
      | COS ->
          _menhir_run_16 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState34
      | ARCTAN ->
          _menhir_run_17 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState34
      | ARCSIN ->
          _menhir_run_18 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState34
      | ARCCOS ->
          _menhir_run_19 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState34
      | _ ->
          _eRR ()
  
  and _menhir_run_35 : type  ttv_stack. ((ttv_stack, _menhir_box_prog) _menhir_cell1_expr as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_prog) _menhir_state -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | TIMES ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_28 _menhir_stack _menhir_lexbuf _menhir_lexer
      | POWER ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_21 _menhir_stack _menhir_lexbuf _menhir_lexer
      | FACT ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_23 _menhir_stack _menhir_lexbuf _menhir_lexer
      | DIV ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_30 _menhir_stack _menhir_lexbuf _menhir_lexer
      | EOF | MINUS | OF | PLUS | RPAREN ->
          let MenhirCell1_expr (_menhir_stack, _menhir_s, e1) = _menhir_stack in
          let e2 = _v in
          let _v = _menhir_action_15 e1 e2 in
          _menhir_goto_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_30 : type  ttv_stack. (ttv_stack, _menhir_box_prog) _menhir_cell1_expr -> _ -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | TAN ->
          _menhir_run_01 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState30
      | SIN ->
          _menhir_run_02 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState30
      | SIMPL ->
          _menhir_run_03 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState30
      | REPR ->
          _menhir_run_04 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState30
      | MINUS ->
          _menhir_run_05 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState30
      | LPAREN ->
          _menhir_run_06 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState30
      | LN ->
          _menhir_run_07 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState30
      | INTGR ->
          _menhir_run_08 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState30
      | INT _v ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let i = _v in
          let _v = _menhir_action_01 i in
          _menhir_run_31 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState30 _tok
      | ID _v ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let x = _v in
          let _v = _menhir_action_02 x in
          _menhir_run_31 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState30 _tok
      | EXP ->
          _menhir_run_12 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState30
      | EXIT ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_22 () in
          _menhir_run_31 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState30 _tok
      | DRVT ->
          _menhir_run_14 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState30
      | COS ->
          _menhir_run_16 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState30
      | ARCTAN ->
          _menhir_run_17 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState30
      | ARCSIN ->
          _menhir_run_18 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState30
      | ARCCOS ->
          _menhir_run_19 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState30
      | _ ->
          _eRR ()
  
  and _menhir_run_31 : type  ttv_stack. ((ttv_stack, _menhir_box_prog) _menhir_cell1_expr as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_prog) _menhir_state -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | POWER ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_21 _menhir_stack _menhir_lexbuf _menhir_lexer
      | FACT ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_23 _menhir_stack _menhir_lexbuf _menhir_lexer
      | DIV | EOF | MINUS | OF | PLUS | RPAREN | TIMES ->
          let MenhirCell1_expr (_menhir_stack, _menhir_s, e1) = _menhir_stack in
          let e2 = _v in
          let _v = _menhir_action_13 e1 e2 in
          _menhir_goto_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_12 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_prog) _menhir_state -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_EXP (_menhir_stack, _menhir_s) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | TAN ->
          _menhir_run_01 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState12
      | SIN ->
          _menhir_run_02 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState12
      | SIMPL ->
          _menhir_run_03 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState12
      | REPR ->
          _menhir_run_04 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState12
      | MINUS ->
          _menhir_run_05 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState12
      | LPAREN ->
          _menhir_run_06 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState12
      | LN ->
          _menhir_run_07 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState12
      | INTGR ->
          _menhir_run_08 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState12
      | INT _v ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let i = _v in
          let _v = _menhir_action_01 i in
          _menhir_run_38 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState12 _tok
      | ID _v ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let x = _v in
          let _v = _menhir_action_02 x in
          _menhir_run_38 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState12 _tok
      | EXP ->
          _menhir_run_12 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState12
      | EXIT ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_22 () in
          _menhir_run_38 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState12 _tok
      | DRVT ->
          _menhir_run_14 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState12
      | COS ->
          _menhir_run_16 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState12
      | ARCTAN ->
          _menhir_run_17 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState12
      | ARCSIN ->
          _menhir_run_18 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState12
      | ARCCOS ->
          _menhir_run_19 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState12
      | _ ->
          _eRR ()
  
  and _menhir_run_38 : type  ttv_stack. ((ttv_stack, _menhir_box_prog) _menhir_cell1_EXP as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_prog) _menhir_state -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | POWER ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_21 _menhir_stack _menhir_lexbuf _menhir_lexer
      | FACT ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_23 _menhir_stack _menhir_lexbuf _menhir_lexer
      | DIV | EOF | MINUS | OF | PLUS | RPAREN | TIMES ->
          let MenhirCell1_EXP (_menhir_stack, _menhir_s) = _menhir_stack in
          let e = _v in
          let _v = _menhir_action_06 e in
          _menhir_goto_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_14 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_prog) _menhir_state -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_DRVT (_menhir_stack, _menhir_s) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | WITH ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | TAN ->
              _menhir_run_01 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState15
          | SIN ->
              _menhir_run_02 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState15
          | SIMPL ->
              _menhir_run_03 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState15
          | REPR ->
              _menhir_run_04 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState15
          | MINUS ->
              _menhir_run_05 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState15
          | LPAREN ->
              _menhir_run_06 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState15
          | LN ->
              _menhir_run_07 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState15
          | INTGR ->
              _menhir_run_08 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState15
          | INT _v ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let i = _v in
              let _v = _menhir_action_01 i in
              _menhir_run_27 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState15 _tok
          | ID _v ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let x = _v in
              let _v = _menhir_action_02 x in
              _menhir_run_27 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState15 _tok
          | EXP ->
              _menhir_run_12 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState15
          | EXIT ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _v = _menhir_action_22 () in
              _menhir_run_27 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState15 _tok
          | DRVT ->
              _menhir_run_14 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState15
          | COS ->
              _menhir_run_16 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState15
          | ARCTAN ->
              _menhir_run_17 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState15
          | ARCSIN ->
              _menhir_run_18 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState15
          | ARCCOS ->
              _menhir_run_19 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState15
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_27 : type  ttv_stack. ((ttv_stack, _menhir_box_prog) _menhir_cell1_DRVT as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_prog) _menhir_state -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | TIMES ->
          _menhir_run_28 _menhir_stack _menhir_lexbuf _menhir_lexer
      | POWER ->
          _menhir_run_21 _menhir_stack _menhir_lexbuf _menhir_lexer
      | PLUS ->
          _menhir_run_32 _menhir_stack _menhir_lexbuf _menhir_lexer
      | OF ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | TAN ->
              _menhir_run_01 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState36
          | SIN ->
              _menhir_run_02 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState36
          | SIMPL ->
              _menhir_run_03 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState36
          | REPR ->
              _menhir_run_04 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState36
          | MINUS ->
              _menhir_run_05 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState36
          | LPAREN ->
              _menhir_run_06 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState36
          | LN ->
              _menhir_run_07 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState36
          | INTGR ->
              _menhir_run_08 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState36
          | INT _v_0 ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let i = _v_0 in
              let _v = _menhir_action_01 i in
              _menhir_run_37 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState36 _tok
          | ID _v_2 ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let x = _v_2 in
              let _v = _menhir_action_02 x in
              _menhir_run_37 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState36 _tok
          | EXP ->
              _menhir_run_12 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState36
          | EXIT ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _v = _menhir_action_22 () in
              _menhir_run_37 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState36 _tok
          | DRVT ->
              _menhir_run_14 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState36
          | COS ->
              _menhir_run_16 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState36
          | ARCTAN ->
              _menhir_run_17 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState36
          | ARCSIN ->
              _menhir_run_18 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState36
          | ARCCOS ->
              _menhir_run_19 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState36
          | _ ->
              _eRR ())
      | MINUS ->
          _menhir_run_34 _menhir_stack _menhir_lexbuf _menhir_lexer
      | FACT ->
          _menhir_run_23 _menhir_stack _menhir_lexbuf _menhir_lexer
      | DIV ->
          _menhir_run_30 _menhir_stack _menhir_lexbuf _menhir_lexer
      | _ ->
          _eRR ()
  
  and _menhir_run_37 : type  ttv_stack. (((ttv_stack, _menhir_box_prog) _menhir_cell1_DRVT, _menhir_box_prog) _menhir_cell1_expr as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_prog) _menhir_state -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | TIMES ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_28 _menhir_stack _menhir_lexbuf _menhir_lexer
      | POWER ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_21 _menhir_stack _menhir_lexbuf _menhir_lexer
      | PLUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_32 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MINUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_34 _menhir_stack _menhir_lexbuf _menhir_lexer
      | FACT ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_23 _menhir_stack _menhir_lexbuf _menhir_lexer
      | DIV ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_30 _menhir_stack _menhir_lexbuf _menhir_lexer
      | EOF | OF | RPAREN ->
          let MenhirCell1_expr (_menhir_stack, _, e1) = _menhir_stack in
          let MenhirCell1_DRVT (_menhir_stack, _menhir_s) = _menhir_stack in
          let e2 = _v in
          let _v = _menhir_action_18 e1 e2 in
          _menhir_goto_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_16 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_prog) _menhir_state -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_COS (_menhir_stack, _menhir_s) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | TAN ->
          _menhir_run_01 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState16
      | SIN ->
          _menhir_run_02 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState16
      | SIMPL ->
          _menhir_run_03 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState16
      | REPR ->
          _menhir_run_04 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState16
      | MINUS ->
          _menhir_run_05 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState16
      | LPAREN ->
          _menhir_run_06 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState16
      | LN ->
          _menhir_run_07 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState16
      | INTGR ->
          _menhir_run_08 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState16
      | INT _v ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let i = _v in
          let _v = _menhir_action_01 i in
          _menhir_run_26 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState16 _tok
      | ID _v ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let x = _v in
          let _v = _menhir_action_02 x in
          _menhir_run_26 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState16 _tok
      | EXP ->
          _menhir_run_12 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState16
      | EXIT ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_22 () in
          _menhir_run_26 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState16 _tok
      | DRVT ->
          _menhir_run_14 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState16
      | COS ->
          _menhir_run_16 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState16
      | ARCTAN ->
          _menhir_run_17 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState16
      | ARCSIN ->
          _menhir_run_18 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState16
      | ARCCOS ->
          _menhir_run_19 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState16
      | _ ->
          _eRR ()
  
  and _menhir_run_26 : type  ttv_stack. ((ttv_stack, _menhir_box_prog) _menhir_cell1_COS as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_prog) _menhir_state -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | POWER ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_21 _menhir_stack _menhir_lexbuf _menhir_lexer
      | FACT ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_23 _menhir_stack _menhir_lexbuf _menhir_lexer
      | DIV | EOF | MINUS | OF | PLUS | RPAREN | TIMES ->
          let MenhirCell1_COS (_menhir_stack, _menhir_s) = _menhir_stack in
          let e = _v in
          let _v = _menhir_action_08 e in
          _menhir_goto_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_17 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_prog) _menhir_state -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_ARCTAN (_menhir_stack, _menhir_s) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | TAN ->
          _menhir_run_01 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState17
      | SIN ->
          _menhir_run_02 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState17
      | SIMPL ->
          _menhir_run_03 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState17
      | REPR ->
          _menhir_run_04 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState17
      | MINUS ->
          _menhir_run_05 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState17
      | LPAREN ->
          _menhir_run_06 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState17
      | LN ->
          _menhir_run_07 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState17
      | INTGR ->
          _menhir_run_08 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState17
      | INT _v ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let i = _v in
          let _v = _menhir_action_01 i in
          _menhir_run_25 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState17 _tok
      | ID _v ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let x = _v in
          let _v = _menhir_action_02 x in
          _menhir_run_25 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState17 _tok
      | EXP ->
          _menhir_run_12 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState17
      | EXIT ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_22 () in
          _menhir_run_25 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState17 _tok
      | DRVT ->
          _menhir_run_14 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState17
      | COS ->
          _menhir_run_16 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState17
      | ARCTAN ->
          _menhir_run_17 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState17
      | ARCSIN ->
          _menhir_run_18 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState17
      | ARCCOS ->
          _menhir_run_19 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState17
      | _ ->
          _eRR ()
  
  and _menhir_run_25 : type  ttv_stack. ((ttv_stack, _menhir_box_prog) _menhir_cell1_ARCTAN as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_prog) _menhir_state -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | POWER ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_21 _menhir_stack _menhir_lexbuf _menhir_lexer
      | FACT ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_23 _menhir_stack _menhir_lexbuf _menhir_lexer
      | DIV | EOF | MINUS | OF | PLUS | RPAREN | TIMES ->
          let MenhirCell1_ARCTAN (_menhir_stack, _menhir_s) = _menhir_stack in
          let e = _v in
          let _v = _menhir_action_12 e in
          _menhir_goto_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_18 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_prog) _menhir_state -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_ARCSIN (_menhir_stack, _menhir_s) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | TAN ->
          _menhir_run_01 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState18
      | SIN ->
          _menhir_run_02 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState18
      | SIMPL ->
          _menhir_run_03 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState18
      | REPR ->
          _menhir_run_04 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState18
      | MINUS ->
          _menhir_run_05 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState18
      | LPAREN ->
          _menhir_run_06 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState18
      | LN ->
          _menhir_run_07 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState18
      | INTGR ->
          _menhir_run_08 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState18
      | INT _v ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let i = _v in
          let _v = _menhir_action_01 i in
          _menhir_run_24 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState18 _tok
      | ID _v ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let x = _v in
          let _v = _menhir_action_02 x in
          _menhir_run_24 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState18 _tok
      | EXP ->
          _menhir_run_12 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState18
      | EXIT ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_22 () in
          _menhir_run_24 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState18 _tok
      | DRVT ->
          _menhir_run_14 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState18
      | COS ->
          _menhir_run_16 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState18
      | ARCTAN ->
          _menhir_run_17 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState18
      | ARCSIN ->
          _menhir_run_18 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState18
      | ARCCOS ->
          _menhir_run_19 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState18
      | _ ->
          _eRR ()
  
  and _menhir_run_24 : type  ttv_stack. ((ttv_stack, _menhir_box_prog) _menhir_cell1_ARCSIN as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_prog) _menhir_state -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | POWER ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_21 _menhir_stack _menhir_lexbuf _menhir_lexer
      | FACT ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_23 _menhir_stack _menhir_lexbuf _menhir_lexer
      | DIV | EOF | MINUS | OF | PLUS | RPAREN | TIMES ->
          let MenhirCell1_ARCSIN (_menhir_stack, _menhir_s) = _menhir_stack in
          let e = _v in
          let _v = _menhir_action_10 e in
          _menhir_goto_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_19 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_prog) _menhir_state -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_ARCCOS (_menhir_stack, _menhir_s) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | TAN ->
          _menhir_run_01 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState19
      | SIN ->
          _menhir_run_02 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState19
      | SIMPL ->
          _menhir_run_03 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState19
      | REPR ->
          _menhir_run_04 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState19
      | MINUS ->
          _menhir_run_05 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState19
      | LPAREN ->
          _menhir_run_06 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState19
      | LN ->
          _menhir_run_07 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState19
      | INTGR ->
          _menhir_run_08 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState19
      | INT _v ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let i = _v in
          let _v = _menhir_action_01 i in
          _menhir_run_20 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState19 _tok
      | ID _v ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let x = _v in
          let _v = _menhir_action_02 x in
          _menhir_run_20 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState19 _tok
      | EXP ->
          _menhir_run_12 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState19
      | EXIT ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_22 () in
          _menhir_run_20 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState19 _tok
      | DRVT ->
          _menhir_run_14 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState19
      | COS ->
          _menhir_run_16 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState19
      | ARCTAN ->
          _menhir_run_17 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState19
      | ARCSIN ->
          _menhir_run_18 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState19
      | ARCCOS ->
          _menhir_run_19 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState19
      | _ ->
          _eRR ()
  
  and _menhir_run_20 : type  ttv_stack. ((ttv_stack, _menhir_box_prog) _menhir_cell1_ARCCOS as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_prog) _menhir_state -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | POWER ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_21 _menhir_stack _menhir_lexbuf _menhir_lexer
      | FACT ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_23 _menhir_stack _menhir_lexbuf _menhir_lexer
      | DIV | EOF | MINUS | OF | PLUS | RPAREN | TIMES ->
          let MenhirCell1_ARCCOS (_menhir_stack, _menhir_s) = _menhir_stack in
          let e = _v in
          let _v = _menhir_action_11 e in
          _menhir_goto_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_49 : type  ttv_stack. ((ttv_stack, _menhir_box_prog) _menhir_cell1_TAN as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_prog) _menhir_state -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | POWER ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_21 _menhir_stack _menhir_lexbuf _menhir_lexer
      | FACT ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_23 _menhir_stack _menhir_lexbuf _menhir_lexer
      | DIV | EOF | MINUS | OF | PLUS | RPAREN | TIMES ->
          let MenhirCell1_TAN (_menhir_stack, _menhir_s) = _menhir_stack in
          let e = _v in
          let _v = _menhir_action_09 e in
          _menhir_goto_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_48 : type  ttv_stack. ((ttv_stack, _menhir_box_prog) _menhir_cell1_SIN as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_prog) _menhir_state -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | POWER ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_21 _menhir_stack _menhir_lexbuf _menhir_lexer
      | FACT ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_23 _menhir_stack _menhir_lexbuf _menhir_lexer
      | DIV | EOF | MINUS | OF | PLUS | RPAREN | TIMES ->
          let MenhirCell1_SIN (_menhir_stack, _menhir_s) = _menhir_stack in
          let e = _v in
          let _v = _menhir_action_07 e in
          _menhir_goto_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_47 : type  ttv_stack. ((ttv_stack, _menhir_box_prog) _menhir_cell1_SIMPL as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_prog) _menhir_state -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | TIMES ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_28 _menhir_stack _menhir_lexbuf _menhir_lexer
      | POWER ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_21 _menhir_stack _menhir_lexbuf _menhir_lexer
      | PLUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_32 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MINUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_34 _menhir_stack _menhir_lexbuf _menhir_lexer
      | FACT ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_23 _menhir_stack _menhir_lexbuf _menhir_lexer
      | DIV ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_30 _menhir_stack _menhir_lexbuf _menhir_lexer
      | EOF | OF | RPAREN ->
          let MenhirCell1_SIMPL (_menhir_stack, _menhir_s) = _menhir_stack in
          let e = _v in
          let _v = _menhir_action_20 e in
          _menhir_goto_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_46 : type  ttv_stack. ((ttv_stack, _menhir_box_prog) _menhir_cell1_REPR as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_prog) _menhir_state -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | TIMES ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_28 _menhir_stack _menhir_lexbuf _menhir_lexer
      | POWER ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_21 _menhir_stack _menhir_lexbuf _menhir_lexer
      | PLUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_32 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MINUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_34 _menhir_stack _menhir_lexbuf _menhir_lexer
      | FACT ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_23 _menhir_stack _menhir_lexbuf _menhir_lexer
      | DIV ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_30 _menhir_stack _menhir_lexbuf _menhir_lexer
      | EOF | OF | RPAREN ->
          let MenhirCell1_REPR (_menhir_stack, _menhir_s) = _menhir_stack in
          let e = _v in
          let _v = _menhir_action_21 e in
          _menhir_goto_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_45 : type  ttv_stack. ((ttv_stack, _menhir_box_prog) _menhir_cell1_MINUS as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_prog) _menhir_state -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | TIMES ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_28 _menhir_stack _menhir_lexbuf _menhir_lexer
      | POWER ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_21 _menhir_stack _menhir_lexbuf _menhir_lexer
      | FACT ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_23 _menhir_stack _menhir_lexbuf _menhir_lexer
      | DIV ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_30 _menhir_stack _menhir_lexbuf _menhir_lexer
      | EOF | MINUS | OF | PLUS | RPAREN ->
          let MenhirCell1_MINUS (_menhir_stack, _menhir_s) = _menhir_stack in
          let e = _v in
          let _v = _menhir_action_17 e in
          _menhir_goto_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_43 : type  ttv_stack. ((ttv_stack, _menhir_box_prog) _menhir_cell1_LPAREN as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_prog) _menhir_state -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | TIMES ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_28 _menhir_stack _menhir_lexbuf _menhir_lexer
      | RPAREN ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_LPAREN (_menhir_stack, _menhir_s) = _menhir_stack in
          let e = _v in
          let _v = _menhir_action_23 e in
          _menhir_goto_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | POWER ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_21 _menhir_stack _menhir_lexbuf _menhir_lexer
      | PLUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_32 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MINUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_34 _menhir_stack _menhir_lexbuf _menhir_lexer
      | FACT ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_23 _menhir_stack _menhir_lexbuf _menhir_lexer
      | DIV ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_30 _menhir_stack _menhir_lexbuf _menhir_lexer
      | _ ->
          _eRR ()
  
  and _menhir_run_42 : type  ttv_stack. ((ttv_stack, _menhir_box_prog) _menhir_cell1_LN as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_prog) _menhir_state -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | POWER ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_21 _menhir_stack _menhir_lexbuf _menhir_lexer
      | FACT ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_23 _menhir_stack _menhir_lexbuf _menhir_lexer
      | DIV | EOF | MINUS | OF | PLUS | RPAREN | TIMES ->
          let MenhirCell1_LN (_menhir_stack, _menhir_s) = _menhir_stack in
          let e = _v in
          let _v = _menhir_action_05 e in
          _menhir_goto_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_41 : type  ttv_stack. (((ttv_stack, _menhir_box_prog) _menhir_cell1_INTGR, _menhir_box_prog) _menhir_cell1_expr as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_prog) _menhir_state -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | TIMES ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_28 _menhir_stack _menhir_lexbuf _menhir_lexer
      | POWER ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_21 _menhir_stack _menhir_lexbuf _menhir_lexer
      | PLUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_32 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MINUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_34 _menhir_stack _menhir_lexbuf _menhir_lexer
      | FACT ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_23 _menhir_stack _menhir_lexbuf _menhir_lexer
      | DIV ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_30 _menhir_stack _menhir_lexbuf _menhir_lexer
      | EOF | OF | RPAREN ->
          let MenhirCell1_expr (_menhir_stack, _, e1) = _menhir_stack in
          let MenhirCell1_INTGR (_menhir_stack, _menhir_s) = _menhir_stack in
          let e2 = _v in
          let _v = _menhir_action_19 e1 e2 in
          _menhir_goto_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  let rec _menhir_run_00 : type  ttv_stack. ttv_stack -> _ -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | TAN ->
          _menhir_run_01 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState00
      | SIN ->
          _menhir_run_02 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState00
      | SIMPL ->
          _menhir_run_03 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState00
      | REPR ->
          _menhir_run_04 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState00
      | MINUS ->
          _menhir_run_05 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState00
      | LPAREN ->
          _menhir_run_06 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState00
      | LN ->
          _menhir_run_07 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState00
      | INTGR ->
          _menhir_run_08 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState00
      | INT _v ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let i = _v in
          let _v = _menhir_action_01 i in
          _menhir_run_51 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState00 _tok
      | ID _v ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let x = _v in
          let _v = _menhir_action_02 x in
          _menhir_run_51 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState00 _tok
      | EXP ->
          _menhir_run_12 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState00
      | EXIT ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_22 () in
          _menhir_run_51 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState00 _tok
      | DRVT ->
          _menhir_run_14 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState00
      | COS ->
          _menhir_run_16 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState00
      | ARCTAN ->
          _menhir_run_17 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState00
      | ARCSIN ->
          _menhir_run_18 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState00
      | ARCCOS ->
          _menhir_run_19 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState00
      | _ ->
          _eRR ()
  
end

let prog =
  fun _menhir_lexer _menhir_lexbuf ->
    let _menhir_stack = () in
    let MenhirBox_prog v = _menhir_run_00 _menhir_stack _menhir_lexbuf _menhir_lexer in
    v
