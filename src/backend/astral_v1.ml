open Astral
open Formula

let init ~backend ~encoding ~dump_queries () =
  Astral.LS.register ();
  Astral.DLS.register ();
  Astral.NLS.register ();
  Astral.Freed.register ();
  Solver.init ~dump_queries ~backend ~encoding ~use_builtin_defs:true ~source:Config.name ()

let convert (f : Formula.t) : SL.t =
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
        let first_next_var = SL.Variable.mk_fresh "next" Sort.loc_ls in
        let first_next = v first_next_var in
        match nls.min_len with
        | 0 -> nls_0
        | 1 -> nls_1
        | _ ->
            SL.mk_gneg nls_1
            @@ SL.mk_exists [ first_next_var ]
                 (SL.mk_star
                    [
                      SL_builtins.mk_pto_nls first ~top ~next:first_next;
                      SL_builtins.mk_ls first_next ~sink:next;
                    ]))
  in
  SL.mk_star (List.map map_atom f)
