open Cil_types
open Cil
open Common

(** This module implements the splitting of assignments and function calls into
    basic instructions *)

let split_call (outer_func : fundec) (lhs_opt : lval option) (func_exp : exp)
    (params : exp list) (location : location) : stmtkind =
  let block = mkBlock [] in
  let _, exp_to_var =
    Block_builder.get_block_builder outer_func block location
  in

  let last_stmt =
    match lhs_opt with
    | Some (Mem inner_exp, _) ->
        (* simplify lhs, to which call result is assigned - last stmt is assignment *)
        let lhs = Option.get lhs_opt in
        let _, orig_name = exp_to_var inner_exp in

        let call_result_var =
          makeLocalVar outer_func ~scope:block
            (get_unique_name orig_name)
            (typeOfLval lhs)
        in

        let call_instr =
          Call (Some (Var call_result_var, NoOffset), func_exp, params, location)
        in
        let new_call_stmt = mkStmtOneInstr ~valid_sid:true call_instr in
        block.bstmts <- new_call_stmt :: block.bstmts;

        Ast_info.mkassign_statement lhs
          (evar ~loc:location call_result_var)
          location
    | lhs ->
        (* recreate the call stmt with simplified params - last stmt is call *)
        let call_instr = Call (lhs, func_exp, params, location) in
        mkStmtOneInstr ~valid_sid:true call_instr
  in

  block.bstmts <- List.rev @@ (last_stmt :: block.bstmts);
  Block block
