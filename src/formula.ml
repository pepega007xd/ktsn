open Astral
open Common

type var = SL.Variable.t
type ls = { first : var; next : var; min_len : int }
type dls = { first : var; last : var; prev : var; next : var; min_len : int }
type nls = { first : var; top : var; next : var; min_len : int }

type pto_target =
  | LS_t of var
  | DLS_t of var * var
  | NLS_t of var * var
  | Generic of (string * var) list

type atom =
  | Eq of var list
  | Distinct of var * var
  | Freed of var
  | PointsTo of var * pto_target
  | LS of ls
  | DLS of dls
  | NLS of nls

type t = atom list

exception Invalid_deref of var * t
exception Invalid_free of var * t

(* state stored by each CFG node in dataflow analysis *)
type state = t list

let nil = SL.Variable.nil

(** Constructors *)

let mk_ls (first : var) (next : var) (min_len : int) =
  LS { first; next; min_len }

let mk_dls (first : var) (last : var) (prev : var) (next : var) (min_len : int)
    =
  DLS { first; last; prev; next; min_len }

let mk_nls (first : var) (top : var) (next : var) (min_len : int) =
  NLS { first; top; next; min_len }

(** Formatters *)

let atom_to_string : atom -> 'a =
  let v var =
    if Config.Print_sort.get () then
      let sort = SL.Variable.get_sort var |> SL.Sort.show in
      SL.Variable.show var ^ ":" ^ sort
    else SL.Variable.show var
  in
  function
  | Eq vars -> vars |> List.map v |> String.concat " = "
  | Distinct (lhs, rhs) -> v lhs ^ " != " ^ v rhs
  | Freed var -> "freed(" ^ v var ^ ")"
  | PointsTo (src, LS_t next) -> v src ^ " -> " ^ v next
  | PointsTo (src, DLS_t (next, prev)) ->
      Format.sprintf "%s -> n:%s,p:%s" (v src) (v next) (v prev)
  | PointsTo (src, NLS_t (top, next)) ->
      Format.sprintf "%s -> t:%s,n:%s" (v src) (v top) (v next)
  | PointsTo (src, Generic vars) ->
      Format.sprintf "%s -> {%s}" (v src)
        (vars
        |> List.map (fun (name, var) -> Format.sprintf "%s:%s" name (v var))
        |> String.concat " ")
  | LS ls -> Format.sprintf "ls_%d+(%s,%s)" ls.min_len (v ls.first) (v ls.next)
  | DLS dls ->
      Format.sprintf "dls_%d+(%s,%s,%s,%s)" dls.min_len (v dls.first)
        (v dls.last) (v dls.prev) (v dls.next)
  | NLS nls ->
      Format.sprintf "nls_%d+(%s,%s,%s)" nls.min_len (v nls.first) (v nls.top)
        (v nls.next)

let pp_atom (fmt : Format.formatter) (atom : atom) =
  Format.fprintf fmt "%s" (atom_to_string atom)

let pp_formula (fmt : Format.formatter) (formula : t) =
  let formula =
    formula |> List.map atom_to_string
    |> List.map (fun s -> "(" ^ s ^ ")")
    |> String.concat " * "
  in
  if formula = "" then Format.fprintf fmt "emp"
  else Format.fprintf fmt "%s" formula

let show_formula (f : t) : unit = Config.Self.warning "FORMULA: %a" pp_formula f

let pp_state (fmt : Format.formatter) (state : state) =
  List.iter
    (fun formula ->
      pp_formula fmt formula;
      Format.fprintf fmt "\n")
    state

(** Conversion to and from Astral type *)

let to_astral (f : t) : SL.t =
  let v = SL.Term.of_var in
  let map_atom = function
    | Eq vars -> SL.mk_eq (List.map v vars)
    | Distinct (lhs, rhs) -> SL.mk_distinct2 (v lhs) (v rhs)
    | Freed var -> SL_builtins.mk_freed (v var)
    | PointsTo (src, LS_t next) -> SL_builtins.mk_pto_ls (v src) ~next:(v next)
    | PointsTo (src, DLS_t (next, prev)) ->
        SL_builtins.mk_pto_dls (v src) ~next:(v next) ~prev:(v prev)
    | PointsTo (src, NLS_t (top, next)) ->
        SL_builtins.mk_pto_nls (v src) ~top:(v top) ~next:(v next)
    | PointsTo (src, Generic vars) ->
        let vars = vars |> List.map snd |> List.map v in
        let struct_def = Types.get_struct_def @@ SL.Variable.get_sort src in
        SL.mk_pto_struct (v src) struct_def vars
    | LS ls -> (
        let first = v ls.first in
        let next = v ls.next in

        let ls_0 = SL_builtins.mk_ls first ~sink:next in
        let ls_1 = SL.mk_star [ ls_0; SL.mk_distinct2 first next ] in

        match ls.min_len with
        | 0 -> ls_0
        | 1 -> ls_1
        | _ -> SL.mk_gneg ls_1 (SL_builtins.mk_pto_ls first ~next))
    | DLS dls -> (
        let first = v dls.first in
        let last = v dls.last in
        let prev = v dls.prev in
        let next = v dls.next in

        let dls_0 =
          SL_builtins.mk_dls first ~sink:next ~root':last ~sink':prev
        in
        let dls_1 = SL.mk_star [ dls_0; SL.mk_distinct2 first next ] in
        let dls_2 =
          SL.mk_star
            [ dls_0; SL.mk_distinct2 first next; SL.mk_distinct2 first last ]
        in

        match dls.min_len with
        | 0 -> dls_0
        | 1 -> dls_1
        | 2 -> dls_2
        | _ ->
            SL.mk_gneg dls_2
              (SL.mk_star
                 [
                   SL_builtins.mk_pto_dls first ~next:last ~prev;
                   SL_builtins.mk_pto_dls last ~next ~prev:first;
                 ]))
    | NLS nls -> (
        let first = v nls.first in
        let top = v nls.top in
        let next = v nls.next in
        let nls_0 = SL_builtins.mk_nls first ~sink:top ~bottom:next in
        let nls_1 = SL.mk_star [ nls_0; SL.mk_distinct2 first top ] in
        let first_next = SL.Term.mk_heap_term MemoryModel.Field.next first in
        match nls.min_len with
        | 0 -> nls_0
        | 1 -> nls_1
        | _ ->
            (* TODO: does this make sense? *)
            SL.mk_gneg nls_1
              (SL.mk_star
                 [
                   SL_builtins.mk_pto_nls first ~top ~next:first_next;
                   SL_builtins.mk_ls first_next ~sink:next;
                 ]))
  in
  SL.mk_star (List.map map_atom f)

let state_to_astral (state : state) : SL.t =
  state |> List.map to_astral |> SL.mk_or

(** Variables *)

let get_vars (f : t) : var list =
  List.concat_map
    (function
      | Eq vars -> vars
      | Distinct (lhs, rhs) -> [ lhs; rhs ]
      | Freed var -> [ var ]
      | PointsTo (src, LS_t next) -> [ src; next ]
      | PointsTo (src, DLS_t (next, prev)) -> [ src; next; prev ]
      | PointsTo (src, NLS_t (top, next)) -> [ src; next; top ]
      | PointsTo (src, Generic vars) -> src :: List.map snd vars
      | LS ls -> [ ls.first; ls.next ]
      | DLS dls -> [ dls.first; dls.last; dls.prev; dls.next ]
      | NLS nls -> [ nls.first; nls.top; nls.next ])
    f

let get_fresh_vars (f : t) : var list =
  f |> get_vars |> List.filter is_fresh_var

let subsitute_in_atom (old_var : var) (new_var : var) : atom -> atom =
  let v (var : var) : var = if var = old_var then new_var else var in

  function
  | Eq vars -> Eq (List.map v vars)
  | Distinct (lhs, rhs) -> Distinct (v lhs, v rhs)
  | Freed var -> Freed (v var)
  | PointsTo (src, LS_t next) -> PointsTo (v src, LS_t (v next))
  | PointsTo (src, DLS_t (next, prev)) ->
      PointsTo (v src, DLS_t (v next, v prev))
  | PointsTo (src, NLS_t (top, next)) -> PointsTo (v src, NLS_t (v top, v next))
  | PointsTo (src, Generic vars) ->
      PointsTo
        (v src, Generic (vars |> List.map (fun (name, var) -> (name, v var))))
  | LS ls -> LS { first = v ls.first; next = v ls.next; min_len = ls.min_len }
  | DLS dls ->
      DLS
        {
          first = v dls.first;
          last = v dls.last;
          prev = v dls.prev;
          next = v dls.next;
          min_len = dls.min_len;
        }
  | NLS nls ->
      NLS
        {
          first = v nls.first;
          top = v nls.top;
          next = v nls.next;
          min_len = nls.min_len;
        }

let substitute (f : t) ~(var : var) ~(by : var) : t =
  List.map (subsitute_in_atom var by) f

let substitute_by_fresh (var : var) : t -> t =
  substitute ~var ~by:(mk_fresh_var_from var)

let swap_vars (var1 : var) (var2 : var) (f : t) =
  let tmp_name = SL.Variable.mk "__tmp_var" SL.Sort.loc_nil in
  f
  |> substitute ~var:var1 ~by:tmp_name
  |> substitute ~var:var2 ~by:var1
  |> substitute ~var:tmp_name ~by:var2

let standardize_fresh_var_names (f : t) : t =
  let vars = f |> get_fresh_vars in
  let names =
    List.mapi
      (fun idx var ->
        SL.Variable.mk ("!" ^ string_of_int idx) (SL.Variable.get_sort var))
      vars
  in
  List.fold_left2 (fun f var by -> substitute ~var ~by f) f vars names

(** Atoms *)

let add_atom (atom : atom) (f : t) : t = atom :: f
let remove_atom (atom : atom) (f : t) : t = f |> List.filter (( <> ) atom)

(** Equivalence classes *)

let get_equiv_classes : t -> var list list =
  List.filter_map (function Eq list -> Some list | _ -> None)

let find_equiv_class (var : var) (f : t) : var list option =
  f |> get_equiv_classes |> List.find_opt (List.mem var)

let map_equiv_classes (fn : var list -> var list) : t -> t =
  List.map (function Eq vars -> Eq (fn vars) | other -> other)

let add_equiv_class (equiv_class : var list) (f : t) =
  add_atom (Eq equiv_class) f

let remove_equiv_class (equiv_class : var list) (f : t) =
  remove_atom (Eq equiv_class) f

let is_eq (lhs : var) (rhs : var) (f : t) : bool =
  if lhs = rhs then true
  else
    f |> find_equiv_class lhs
    |> Option.map (List.exists (( = ) rhs))
    |> Option.value ~default:false

(** Spatial atoms *)

let is_spatial_atom : atom -> bool = function
  | PointsTo _ | LS _ | DLS _ | NLS _ -> true
  | _ -> false

let get_spatial_atoms : t -> t = List.filter is_spatial_atom

let is_spatial_source (src : var) : atom -> bool = function
  | PointsTo (var, _) -> src = var
  | LS ls -> ls.first = src
  | DLS dls -> dls.first = src || dls.last = src
  | NLS nls -> nls.first = src
  | _ -> false

let is_spatial_source_first (src : var) : atom -> bool = function
  | PointsTo (var, _) -> src = var
  | LS ls -> ls.first = src
  | DLS dls -> dls.first = src
  | NLS nls -> nls.first = src
  | _ -> false

let make_var_explicit_src (var : var) (f : t) : t =
  find_equiv_class var f |> function
  | Some equiv_class ->
      let current_src =
        List.find_opt
          (fun src -> List.exists (is_spatial_source src) f)
          equiv_class
      in
      Option.map (fun current_src -> swap_vars current_src var f) current_src
      |> Option.value ~default:f
  | None -> f

let get_spatial_atom_from_opt (src : var) (f : t) : atom option =
  f |> make_var_explicit_src src |> List.find_opt (is_spatial_source src)

let get_spatial_atom_from_first_opt (src : var) (f : t) : atom option =
  f |> make_var_explicit_src src |> List.find_opt (is_spatial_source_first src)

let get_spatial_atom_from (src : var) (f : t) : atom =
  get_spatial_atom_from_opt src f |> function
  | Some atom -> atom
  | None -> raise @@ Invalid_deref (src, f)

let get_target_of_atom (field : Types.field_type) (atom : atom) : var =
  match (atom, field) with
  | PointsTo (_, LS_t next), Next -> next
  | PointsTo (_, DLS_t (next, _)), Next -> next
  | PointsTo (_, DLS_t (_, prev)), Prev -> prev
  | PointsTo (_, NLS_t (top, _)), Top -> top
  | PointsTo (_, NLS_t (_, next)), Next -> next
  | PointsTo (_, Generic vars), Other field -> List.assoc field vars
  | LS ls, Next -> ls.next
  | DLS dls, Next -> dls.next
  | DLS dls, Prev -> dls.prev
  | NLS nls, Top -> nls.top
  | NLS nls, Next -> nls.next
  | _ -> assert false

let get_targets_of_atom : atom -> var list = function
  | PointsTo (_, LS_t next) -> [ next ]
  | PointsTo (_, DLS_t (next, prev)) -> [ next; prev ]
  | PointsTo (_, NLS_t (top, next)) -> [ top; next ]
  | PointsTo (_, Generic vars) -> List.map snd vars
  | LS ls -> [ ls.next ]
  | DLS dls -> [ dls.prev; dls.next ]
  | NLS nls -> [ nls.top; nls.next ]
  | _ -> assert false

let is_spatial_target (target : var) (f : t) : bool =
  f |> get_spatial_atoms
  |> List.exists (fun atom ->
         get_targets_of_atom atom |> List.exists (fun var -> is_eq target var f))

let get_spatial_target (src : var) (field : Types.field_type) (f : t) :
    var option =
  get_spatial_atom_from_opt src f |> Option.map (get_target_of_atom field)

let remove_spatial_from (src : var) (f : t) : t =
  get_spatial_atom_from_opt src f |> function
  | Some original_atom -> remove_atom original_atom f
  | None -> f

let change_pto_target (src : var) (field : Types.field_type) (new_target : var)
    (f : t) : t =
  let f = make_var_explicit_src src f in
  let old_struct =
    match get_spatial_atom_from src f with
    | PointsTo (_, old_struct) -> old_struct
    | _ -> assert false
  in
  let new_struct =
    match (field, old_struct) with
    | Next, LS_t _ -> LS_t new_target
    | Next, DLS_t (_, prev) -> DLS_t (new_target, prev)
    | Next, NLS_t (top, _) -> NLS_t (top, new_target)
    | Prev, DLS_t (next, _) -> DLS_t (next, new_target)
    | Top, NLS_t (_, next) -> NLS_t (new_target, next)
    | Other field, Generic vars ->
        Generic ((field, new_target) :: List.remove_assoc field vars)
    | _ -> assert false
  in
  f |> remove_spatial_from src |> add_atom (PointsTo (src, new_struct))

let get_spatial_atom_min_length : atom -> int = function
  | LS ls -> ls.min_len
  | DLS dls -> dls.min_len
  | NLS nls -> nls.min_len
  | PointsTo _ -> 1
  | _ -> assert false

let assert_allocated (var : var) (f : t) : unit =
  ignore @@ get_spatial_atom_from var f

let pto_to_list : atom -> atom = function
  | PointsTo (first, LS_t next) -> LS { first; next; min_len = 1 }
  | PointsTo (src, DLS_t (next, prev)) ->
      DLS { first = src; last = src; next; prev; min_len = 1 }
  | PointsTo (first, NLS_t (top, next)) -> NLS { first; top; next; min_len = 1 }
  | other -> other

(** Pure atoms *)

let add_eq (lhs : var) (rhs : var) (f : t) : t =
  let lhs_class = find_equiv_class lhs f in
  let rhs_class = find_equiv_class rhs f in

  match (lhs_class, rhs_class) with
  (* both variables are already in the same equiv class - do nothing *)
  | Some lhs_class, Some rhs_class when lhs_class = rhs_class -> f
  (* each variable is already in a different equiv class - merge classes *)
  | Some lhs_class, Some rhs_class ->
      f
      |> remove_equiv_class lhs_class
      |> remove_equiv_class rhs_class
      |> add_equiv_class (lhs_class @ rhs_class)
  (* one of the variables is in no existing class - add it to the existing one *)
  | Some lhs_class, None ->
      f |> remove_equiv_class lhs_class |> add_equiv_class (rhs :: lhs_class)
  | None, Some rhs_class ->
      f |> remove_equiv_class rhs_class |> add_equiv_class (lhs :: rhs_class)
  (* no variable is in an existing class - create a new class *)
  | _ -> f |> add_equiv_class [ lhs; rhs ]

let add_distinct (lhs : var) (rhs : var) (f : t) : t =
  let try_increase_bound lhs rhs =
    let f = make_var_explicit_src lhs f in
    match get_spatial_atom_from_opt lhs f with
    | Some (LS ls) when ls.min_len = 0 && is_eq ls.next rhs f ->
        Some (f |> remove_atom (LS ls) |> add_atom (LS { ls with min_len = 1 }))
    (* first != last means length at least 2 *)
    | Some (DLS dls) when dls.min_len < 2 && is_eq dls.last rhs f ->
        Some
          (f |> remove_atom (DLS dls) |> add_atom (DLS { dls with min_len = 2 }))
    (* first != next or first != prev means length at least 1 *)
    | Some (DLS dls)
      when dls.min_len = 0 && (is_eq dls.next rhs f || is_eq dls.prev rhs f) ->
        Some
          (f |> remove_atom (DLS dls) |> add_atom (DLS { dls with min_len = 1 }))
    | Some (NLS nls) when nls.min_len = 0 && is_eq nls.top rhs f ->
        Some
          (f |> remove_atom (NLS nls) |> add_atom (NLS { nls with min_len = 2 }))
    | _ -> None
  in

  match (try_increase_bound lhs rhs, try_increase_bound rhs lhs) with
  | Some f, _ -> f
  | _, Some f -> f
  | _ -> f |> add_atom (Distinct (lhs, rhs))

(** Materialization *)

(** transforms [formula] so that [var] is a part of a points-to atom, not a list
    segment, multiple formulas can be produced, representing different lengths
    of [ls] (1, 2+) *)
let rec materialize (var : var) (f : t) : t list =
  let fresh_var = mk_fresh_var_from var in
  let f = make_var_explicit_src var f in
  let old_atom = get_spatial_atom_from var f in
  let f = f |> remove_atom old_atom in

  match old_atom with
  | PointsTo _ -> [ add_atom old_atom f ]
  (* ls has minimum length greater than zero -> just decrement and split off PointsTo *)
  | LS ls when ls.min_len > 0 ->
      [
        f
        |> add_atom (PointsTo (var, LS_t fresh_var))
        |> add_atom @@ mk_ls fresh_var ls.next (ls.min_len - 1);
      ]
  (* ls has minimum length equal to zero -> case split to 0 and 1+ *)
  | LS ls ->
      (* case where ls has length 1+ *)
      (f
      |> add_atom (PointsTo (var, LS_t fresh_var))
      |> add_atom @@ mk_ls fresh_var ls.next 0)
      (* cases where ls has length 0 *)
      :: (f |> add_eq ls.first ls.next |> materialize var)
  (* cases where DLS has minimum length of at least one *)
  | DLS dls when dls.min_len > 0 && var = dls.first ->
      [
        f
        |> add_atom (PointsTo (var, DLS_t (fresh_var, dls.prev)))
        |> add_atom @@ mk_dls fresh_var dls.last var dls.next (dls.min_len - 1);
      ]
  | DLS dls when dls.min_len > 0 && var = dls.last ->
      [
        f
        |> add_atom (PointsTo (var, DLS_t (dls.next, fresh_var)))
        |> add_atom @@ mk_dls dls.first fresh_var dls.prev var (dls.min_len - 1);
      ]
  (* cases where DLS has minimum length of zero -> case split *)
  | DLS dls when var = dls.first ->
      (* length 1+ case *)
      (f
      |> add_atom (PointsTo (var, DLS_t (fresh_var, dls.prev)))
      |> add_atom @@ mk_dls fresh_var dls.last var dls.next 0)
      (* length 0 cases *)
      :: (f |> add_eq dls.first dls.last |> add_eq dls.first dls.next
        |> add_eq dls.last dls.prev |> materialize var)
  | DLS dls when var = dls.last ->
      (f
      |> add_atom (PointsTo (var, DLS_t (dls.next, fresh_var)))
      |> add_atom @@ mk_dls dls.first fresh_var dls.prev var 0)
      :: (f |> add_eq dls.first dls.last |> add_eq dls.first dls.next
        |> add_eq dls.last dls.prev |> materialize var)
  (* case where NLS has minimum length of at least one *)
  | NLS nls when nls.min_len > 0 ->
      (* materalization of NLS produces a LS_0+ from fresh_var to `nls.next` *)
      let fresh_ls = SL.Variable.mk_fresh "fresh" Sort.loc_ls in
      [
        f
        |> add_atom (PointsTo (var, NLS_t (fresh_var, fresh_ls)))
        |> add_atom @@ mk_ls fresh_ls nls.next 0
        |> add_atom @@ mk_nls fresh_var nls.top nls.next (nls.min_len - 1);
      ]
  (* case where NLS has minimum length == 0 *)
  | NLS nls ->
      let fresh_ls = SL.Variable.mk_fresh "fresh" Sort.loc_ls in
      (* length 1+ case *)
      (f
      |> add_atom (PointsTo (var, NLS_t (fresh_var, fresh_ls)))
      |> add_atom @@ mk_ls fresh_ls nls.next 0
      |> add_atom @@ mk_nls fresh_var nls.top nls.next 0)
      (* length 0 cases *)
      :: (f |> add_eq nls.first nls.top |> materialize var)
  | _ -> assert false

(** Miscellaneous *)

let rec split_by_reachability_from ((spatial, rest) : t * t) (var : var) : t * t
    =
  let rest = make_var_explicit_src var rest in
  get_spatial_atom_from_opt var rest |> function
  | Some atom ->
      let targets = get_targets_of_atom atom in
      List.fold_left split_by_reachability_from
        (atom :: spatial, remove_atom atom rest)
        targets
  | None -> (spatial, rest)

let split_by_reachability (vars : var list) (f : t) : t * t =
  let reachable_spatials, rest =
    List.fold_left split_by_reachability_from ([], f) vars
  in

  (* always include the function args and nil so that they remain in equiv classes *)
  let reachable_vars = (nil :: vars) @ get_vars reachable_spatials in

  let reachable_equiv_classes =
    rest
    |> List.filter (function Eq _ -> true | _ -> false)
    |> map_equiv_classes (List.filter (fun var -> List.mem var reachable_vars))
  in

  let reachable_distincts =
    List.filter
      (function
        | Distinct (lhs, rhs) ->
            List.mem lhs reachable_vars && List.mem rhs reachable_vars
        | _ -> false)
      rest
  in

  let reachable =
    reachable_equiv_classes @ reachable_spatials @ reachable_distincts
  in
  let unreachable =
    List.filter (fun atom -> not @@ List.mem atom reachable) rest
  in
  (reachable, unreachable)

(** returns true when variable appears only once in the formula, [Distinct] and
    [Freed] atoms are ignored *)
let count_relevant_occurences (var : var) (f : t) : int =
  f
  |> List.filter (function Distinct _ | Freed _ -> false | _ -> true)
  |> get_vars |> Common.list_count var

let canonicalize (f : t) : t =
  let c = SL.Variable.compare in
  let vars = f |> get_vars |> List.sort_uniq c in

  List.fold_left (fun f var -> make_var_explicit_src var f) f vars
  |> List.map (function
       | Eq vars -> Eq (List.sort_uniq c vars)
       | Distinct (lhs, rhs) ->
           if c lhs rhs > 0 then Distinct (lhs, rhs) else Distinct (rhs, lhs)
       | c -> c)
  |> List.sort_uniq compare

let canonicalize_state (state : state) : state =
  state |> List.map canonicalize |> List.sort_uniq compare

let sum_of_bounds (add_ptos : bool) (f : t) : int =
  let result =
    f
    |> List.map (function
         | LS { min_len; _ } | DLS { min_len; _ } | NLS { min_len; _ } ->
             min_len
         | PointsTo _ when add_ptos -> 1
         | _ -> 0)
    |> List.fold_left ( + ) 0
  in
  if List.exists (function LS _ | DLS _ | NLS _ -> true | _ -> false) f then
    result - 1
  else result

let compare_bounds (lhs : t) (rhs : t) : int =
  let with_ptos = sum_of_bounds true lhs - sum_of_bounds true rhs in
  let without_ptos = sum_of_bounds false lhs - sum_of_bounds false rhs in
  if with_ptos = 0 then without_ptos else with_ptos
