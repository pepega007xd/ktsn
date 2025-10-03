open Cil
open Cil_types
open Common
open Astral
open Constants

(** This module implements multiple simple preprocessing passes, and serves as
    the entrypoint to preprocessing *)

let remove_casts =
  object
    inherit nopCilVisitor

    method! vexpr expr =
      match expr.enode with
      | CastE (_, inner) -> ChangeDoChildrenPost (inner, fun x -> x)
      | _ -> DoChildren
  end

let replace_constants =
  object
    inherit Visitor.frama_c_inplace

    method! vexpr (exp : exp) =
      match exp.enode with
      | CastE (typ, exp) when Ast_types.is_ptr typ && is_nullptr exp ->
          ChangeTo (evar nullptr_var)
      | Const _ | SizeOf _ | SizeOfE _ | SizeOfStr _ ->
          ChangeTo (evar const_var)
      | BinOp ((Eq | Ne), _, _, _) | UnOp (LNot, _, _) -> DoChildren
      | (BinOp (_, _, _, typ) | UnOp (_, _, typ))
        when not @@ Types.is_relevant_type typ ->
          ChangeTo (evar const_var)
      | _ -> DoChildren
  end

(** Converts variable initializations to simple assignments *)
let remove_local_init =
  object
    inherit Visitor.frama_c_inplace

    method! vinst (instr : instr) =
      match instr with
      | Local_init (varinfo, local_init, location) -> (
          let lval = (Var varinfo, NoOffset) in
          match local_init with
          (* Type var = exp; *)
          | AssignInit (SingleInit exp) ->
              ChangeTo [ Ast_info.mkassign lval exp location ]
          (* Type var = func(); *)
          | ConsInit (func, params, Plain_func) ->
              ChangeTo [ Call (Some lval, evar func, params, location) ]
          | _ -> fail "Unsupported instruction: %a" Printer.pp_instr instr)
      | _ -> SkipChildren
  end

let remove_noop_assignments =
  object
    inherit Visitor.frama_c_inplace

    method! vinst (instr : instr) =
      match instr with
      | Set (lhs, { enode = Lval rhs; _ }, location) ->
          if Cil_datatype.LvalStructEq.equal lhs rhs then
            ChangeTo [ Skip location ]
          else SkipChildren
      | _ -> SkipChildren
  end

(** Gives every variable a globally unique name *)
let unique_names (functions : fundec list) =
  object (self)
    inherit Visitor.frama_c_inplace

    method! vvrbl (var : varinfo) =
      let fundec = self#current_func |> Option.get in
      let other_funcs =
        List.filter (fun f -> f.svar.vid <> fundec.svar.vid) functions
      in
      List.iter (fun f -> refresh_local_name f var) other_funcs;
      SkipChildren
  end

let remove_irrelevant_call_args =
  object
    inherit Visitor.frama_c_inplace

    method! vinst (instr : instr) =
      match instr with
      | Call (lval_opt, fn, args, location) ->
          let args =
            List.filter (fun arg -> typeOf arg |> Types.is_relevant_type) args
          in
          ChangeTo [ Call (lval_opt, fn, args, location) ]
      | _ -> SkipChildren
  end

let remove_not_operator =
  object
    inherit Visitor.frama_c_inplace

    method! vstmt_aux (stmt : stmt) =
      match stmt.skind with
      | If (condition, th, el, location) ->
          let new_if condition =
            If (new_exp ~loc:location condition, th, el, location)
          in
          let new_stmtkind =
            match condition.enode with
            | UnOp (LNot, exp, _) -> (
                match exp.enode with
                | UnOp (LNot, { enode; _ }, _) -> new_if enode
                | BinOp (Eq, lhs, rhs, typ) ->
                    new_if (BinOp (Ne, lhs, rhs, typ))
                | BinOp (Ne, lhs, rhs, typ) ->
                    new_if (BinOp (Eq, lhs, rhs, typ))
                | _ -> stmt.skind)
            | _ -> stmt.skind
          in
          stmt.skind <- new_stmtkind;
          DoChildren
      | _ -> DoChildren
  end

(** Removes conditionals that can be statically evaluated *)
let remove_const_conditions =
  object
    inherit Visitor.frama_c_inplace

    method! vstmt_aux (stmt : stmt) =
      match stmt.skind with
      | If (condition, th, el, _) ->
          let new_stmtkind =
            match constFoldToInt condition with
            | Some n when Z.equal Z.one n -> Block th
            | Some n when Z.equal Z.zero n -> Block el
            | _ -> stmt.skind
          in
          stmt.skind <- new_stmtkind;
          DoChildren
      | _ -> DoChildren
  end

let stack_allocated_vars : SL.Variable.t list ref = ref []

let collect_stack_allocated_vars =
  object
    inherit Visitor.frama_c_inplace

    method! vinst (instr : instr) =
      match instr with
      | Set ((Var lhs, NoOffset), { enode = AddrOf (Var _, NoOffset); _ }, _) ->
          stack_allocated_vars :=
            Types.varinfo_to_var lhs :: !stack_allocated_vars;
          SkipChildren
      | _ -> SkipChildren
  end

(** This function runs all preprocessing passes in order *)
let preprocess () =
  let file = Ast.get () in

  uniqueVarNames file;

  let functions =
    List.filter_map
      (function GFun (func, _) -> Some func | _ -> None)
      file.globals
  in

  Visitor.visitFramacFileFunctions (unique_names functions) file;
  Visitor.visitFramacFileFunctions remove_const_conditions file;
  Visitor.visitFramacFileFunctions replace_constants file;
  visitCilFileFunctions remove_casts file;
  Visitor.visitFramacFileFunctions remove_local_init file;
  Visitor.visitFramacFileFunctions remove_irrelevant_call_args file;
  Visitor.visitFramacFileFunctions remove_noop_assignments file;
  Visitor.visitFramacFileFunctions remove_not_operator file;
  Visitor.visitFramacFileFunctions Condition_split.collect_nondet_int_vars file;
  Visitor.visitFramacFileFunctions Condition_split.split_conditions file;
  Visitor.visitFramacFileFunctions remove_noop_assignments file;
  Types.process_types file;
  Visitor.visitFramacFileFunctions collect_stack_allocated_vars file;

  (* this must run after adding statements *)
  Ast.mark_as_changed ();
  Cfg.clearFileCFG file;
  Cfg.computeFileCFG file;
  Async.yield ()
