open Cil
open Cil_types
open Astral
open Constants
open Common

(** This module implements the analysis of C types that determines, which list
    types they represent *)

(** Classification of structs *)
type struct_type = Sll | Dll | Nl | Struct

(** Classification of struct fields *)
type field_type = Next | Prev | Top | Other of string | Data

let pp_typ_node fmt tnode = Cil_printer.pp_typ fmt { tnode; tattr = [] }

let rec is_relevant_type (typ : typ) : bool =
  match Ast_types.unroll_deep_node typ with
  | TComp _ -> true
  | TPtr inner -> is_relevant_type inner
  | _ -> false

let is_relevant_var (var : varinfo) =
  is_relevant_type var.vtype || var.vname = Constants.null_var_name

let get_struct_pointer_fields (structure : compinfo) : fieldinfo list =
  structure.cfields |> Option.get
  |> List.filter (fun field -> is_relevant_type field.ftype)

let rec get_self_and_sll_fields (structure : compinfo) :
    fieldinfo list * fieldinfo list =
  let self_pointers, other_pointers =
    structure |> get_struct_pointer_fields
    |> List.partition (fun field ->
           match Ast_types.unroll_deep_node field.ftype with
           | TPtr { tnode = TComp target_struct; _ } ->
               target_struct.ckey = structure.ckey
           | _ -> false)
  in
  let sll_pointers =
    List.filter
      (fun field ->
        match Ast_types.unroll_deep_node field.ftype with
        | TPtr { tnode = TComp structure; _ } -> get_struct_type structure = Sll
        | _ -> false)
      other_pointers
  in
  (self_pointers, sll_pointers)

(** Determines, which list type a structure represents based on its fields *)
and get_struct_type (structure : compinfo) : struct_type =
  let self_pointers, sll_pointers = get_self_and_sll_fields structure in

  match (self_pointers, sll_pointers) with
  | [ _ ], [] -> Sll
  | [ _ ], [ _ ] -> Nl
  | [ _; _ ], [] -> Dll
  | _ -> Struct

(** Determines the type of field in the context of lists *)
let get_field_type (field : fieldinfo) : field_type =
  let self_pointers, sll_pointers = get_self_and_sll_fields field.fcomp in

  match (self_pointers, sll_pointers) with
  (* SLL *)
  | [ next ], [] when field.forder = next.forder -> Next
  (* DLL *)
  | [ next; _ ], [] when field.forder = next.forder -> Next
  | [ _; prev ], [] when field.forder = prev.forder -> Prev
  (* NL *)
  | [ top ], [ _ ] when field.forder = top.forder -> Top
  | [ _ ], [ next ] when field.forder = next.forder -> Next
  | _ -> if is_relevant_type field.ftype then Other field.fname else Data

let type_info : (typ_node, Sort.t * MemoryModel.StructDef.t) Hashtbl.t =
  Hashtbl.create 113

let rec get_type_info (typ : typ) : Sort.t * MemoryModel.StructDef.t =
  let typ = Ast_types.unroll_deep_node typ in
  Hashtbl.find_opt type_info typ |> function
  | Some result -> result
  | None -> (
      let dummy_struct_def = MemoryModel.StructDef.mk "dummy_struct_def" [] in
      match typ with
      | TPtr { tnode = TComp structure; _ } -> (
          match get_struct_type structure with
          | Sll -> (SL_builtins.loc_ls, dummy_struct_def)
          | Dll -> (SL_builtins.loc_dls, dummy_struct_def)
          | Nl -> (SL_builtins.loc_nls, dummy_struct_def)
          | Struct ->
              let name = structure.cname in
              let sort = Sort.mk_loc name in
              let fields =
                structure |> get_struct_pointer_fields
                |> List.map (fun field ->
                       let sort = field.ftype |> get_type_info |> fst in
                       MemoryModel0.Field.mk field.fname sort)
              in
              let struct_def = MemoryModel.StructDef.mk name fields in
              let result = (sort, struct_def) in
              Hashtbl.add type_info typ result;
              result)
      | TPtr inner ->
          let name = Common.get_unique_name "ptr2ptr" in
          let sort = Sort.mk_loc name in
          let inner_sort = get_type_info inner |> fst in
          let field =
            MemoryModel0.Field.mk Constants.ptr_field_name inner_sort
          in
          let struct_def = MemoryModel.StructDef.mk name [ field ] in
          let result = (sort, struct_def) in
          Hashtbl.add type_info typ result;
          result
      | other -> fail "invalid type: %a" pp_typ_node other)

let get_struct_def (sort : Sort.t) : MemoryModel.StructDef.t =
  Hashtbl.to_seq_values type_info
  |> Seq.find (fun (s, _) -> sort = s)
  |> Option.get |> snd

(** Converts the type of a variable into its sort, and creates an SL variable *)
let varinfo_to_var (varinfo : Cil_types.varinfo) : SL.Variable.t =
  match varinfo.vname with
  | name when name = null_var_name -> SL.Variable.nil
  | name when name = const_var_name -> fail "_const in varinfo_to_var"
  | name when name = nondet_var_name -> fail "_nondet in varinfo_to_var"
  | _ when not @@ is_relevant_var varinfo ->
      fail "invalid type in varinfo_to_var: %a" Printer.pp_varinfo varinfo
  | _ ->
      let sort = varinfo.vtype |> get_type_info |> fst in
      SL.Variable.mk varinfo.vname sort

(** Memoizes list types inside [get_type_info] *)
let process_types =
  object
    inherit Visitor.frama_c_inplace

    method! vtype (typ : typ) =
      if is_relevant_type typ then ignore @@ get_type_info typ;
      SkipChildren
  end

(** Generates struct definitions for generic structs and sets them in the solver*)
let process_types (file : file) =
  Visitor.visitFramacFileFunctions process_types file;

  let heap_sort =
    Hashtbl.to_seq_values type_info |> List.of_seq |> HeapSort.of_list
  in
  Common.solver := Solver.add_heap_sort heap_sort !Common.solver
