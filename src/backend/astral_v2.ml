open Astral
open SL_builtins
open Astral.Lists
open Formula

(** TODO: do not used SID directly *)
let init ~backend ~encoding ~dump_queries () =
  GlobalSID.register_user_defined ls;
  GlobalSID.register_user_defined dls;
  GlobalSID.register_user_defined nls;
  Freed.register ();

  let open SL_builtins in
  let heap_sort =
    HeapSort.of_list
      [
        (loc_ls, LS.struct_ls);
        (loc_dls, DLS.struct_dls);
        (loc_nls, NLS.struct_nls);
      ]
  in
  Solver.init ~dump_queries ~backend ~encoding ~quantifier_encoding:`Direct
    ~use_builtin_defs:false ~source:"ktsn" ()
  |> Solver.add_heap_sort heap_sort

let[@warning "-8"] convert f =
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
        let ls_2 =
          let n = SL.Term.mk_fresh_var "n" loc_ls in
          (*SL.mk_exists' [loc_ls] (fun [n] ->*)
          SL.mk_star
            [
              SL_builtins.mk_pto_ls first ~next:n;
              SL.mk_predicate "ls" [ n; next ];
              SL.mk_distinct [ first; n; next ];
            ]
          (* ) *)
        in
        match ls.min_len with 0 -> ls_0 | 1 -> ls_1 | _ -> ls_2)
    | DLS dls -> (
        let first = v dls.first in
        let last = v dls.last in
        let prev = v dls.prev in
        let next = v dls.next in

        let dls_0 = SL.mk_predicate "dls" [ first; next; last; prev ] in
        let dls_1 = SL.mk_star [ dls_0; SL.mk_distinct2 first next ] in
        let dls_2 = SL.mk_star [ dls_1; SL.mk_distinct2 first last ] in
        let dls_3 =
          let n = SL.Term.mk_fresh_var "n" loc_dls in
          (*SL.mk_exists' [loc_dls] (fun [n] ->*)
          SL.mk_star
            [
              SL_builtins.mk_pto_dls first ~next:n ~prev;
              SL.mk_predicate "dls" [ n; next; last; first ];
              SL.mk_distinct2 n next;
              SL.mk_distinct2 last first;
              SL.mk_distinct2 first next;
            ]
          (* ) *)
        in

        match dls.min_len with
        | 0 -> dls_0
        | 1 -> dls_1
        | 2 -> dls_2
        | _ -> dls_3)
    | NLS nls -> (
        let first = v nls.first in
        let top = v nls.top in
        let next = v nls.next in
        let nls_0 = SL_builtins.mk_nls first ~sink:top ~bottom:next in
        let nls_1 = SL.mk_star [ nls_0; SL.mk_distinct2 first top ] in
        let nls_2 =
          let t = SL.Term.mk_fresh_var "t" loc_nls in
          let n = SL.Term.mk_fresh_var "n" loc_ls in
          (*SL.mk_exists' [loc_nls; loc_ls] (fun [t; n] ->*)
          SL.mk_star
            [
              SL_builtins.mk_pto_nls first ~top:t ~next:n;
              SL.mk_predicate "nls" [ t; top; next ];
              SL.mk_predicate "ls" [ n; next ];
              SL.mk_distinct2 first next;
              SL.mk_distinct2 t next;
            ]
          (* ) *)
        in
        match nls.min_len with 0 -> nls_0 | 1 -> nls_1 | _ -> nls_2)
  in
  SL.mk_star (List.map map_atom f)
