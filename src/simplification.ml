open Astral
open Common

(** removes fresh variables by substituting them with other variables in their
    equivalence class *)
let reduce_equiv_classes (formula : Formula.t) : Formula.t =
  let remove_fresh_vars (formula : Formula.t) (equiv_class : Formula.var list) :
      Formula.t =
    let fresh_vars, program_vars = List.partition is_fresh_var equiv_class in

    (* variable, to which all fresh vars will be changed *)
    let substitution_var =
      match (List.mem Formula.nil equiv_class, program_vars, fresh_vars) with
      (* preferrably, pick nil *)
      | true, _, _ -> Formula.nil
      (* if not possible, pick a program variable *)
      | _, program_var :: _, _ -> program_var
      (* if not possible, pick any fresh variable *)
      | _, _, fresh_var :: _ -> fresh_var
      | _ -> assert false
    in

    List.fold_left
      (fun formula to_remove ->
        Formula.substitute formula ~var:to_remove ~by:substitution_var)
      formula fresh_vars
  in
  formula |> Formula.get_equiv_classes
  (* filter out equivalence classes with less than two members *)
  |> List.filter (function [] | [ _ ] -> false | _ -> true)
  |> List.fold_left remove_fresh_vars formula
  (* remove multiple occurences of a single variable 
     in an equiv class created by a substitution *)
  |> Formula.map_equiv_classes @@ List.sort_uniq SL.Variable.compare
  (* filter out equivalence classes reduced to <2 members *)
  |> List.filter (function Formula.Eq ([] | [ _ ]) -> false | _ -> true)

let remove_irrelevant_vars (formula : Formula.t) : Formula.t =
  let is_relevant_var (var : Formula.var) : bool =
    (not @@ is_fresh_var var)
    || Formula.count_relevant_occurences var formula > 0
  in
  List.filter
    (function
      | Formula.Distinct (lhs, rhs) ->
          is_relevant_var lhs && is_relevant_var rhs
      | Formula.Freed var -> is_relevant_var var
      | _ -> true)
    formula

(** removes all spatial atoms where the source variable doesn't appear anywhere
    else in the formula *)
let remove_leaks (formula : Formula.t) : Formula.t =
  let is_fresh_unique (var : Formula.var) : bool =
    is_fresh_var var && Formula.count_relevant_occurences var formula = 1
  in
  let junk_atoms, valid_atoms =
    List.partition
      (function
        | Formula.PointsTo (src, _) -> is_fresh_unique src
        | Formula.LS ls -> is_fresh_unique ls.first
        | Formula.DLS dls ->
            is_fresh_unique dls.first && is_fresh_unique dls.last
        | Formula.NLS nls -> is_fresh_unique nls.first
        | _ -> false)
      formula
  in

  if List.is_empty junk_atoms then formula
  else (
    List.iter
      (fun atom ->
        if Formula.bound_of_atom true atom > 0 then
          Formula.report_bug (Invalid_memtrack (atom, formula)))
      junk_atoms;
    valid_atoms)

let convert_vars_to_fresh (vars : Formula.var list) (formula : Formula.t) :
    Formula.t =
  List.fold_left
    (fun formula var -> Formula.substitute_by_fresh var formula)
    formula vars

(** Iterates through all pairs of formulas and joins formulas that differ in a
    length bound of a single spatial atom *)
let generalize_similar_formulas (lhs : Formula.t) (rhs : Formula.t) :
    Formula.t option =
  let rest, first = List.partition (fun atom -> List.mem atom rhs) lhs in
  let second = List.filter (fun atom -> not @@ List.mem atom lhs) rhs in

  match (first, second) with
  | [ first ], [ second ] ->
      let new_atom =
        match (Formula.pto_to_list first, Formula.pto_to_list second) with
        | LS lhs, LS rhs
          when { lhs with min_len = 0 } = { rhs with min_len = 0 } ->
            Some (Formula.LS { lhs with min_len = min lhs.min_len rhs.min_len })
        | DLS lhs, DLS rhs
          when { lhs with min_len = 0 } = { rhs with min_len = 0 } ->
            Some
              (Formula.DLS { lhs with min_len = min lhs.min_len rhs.min_len })
        | NLS lhs, NLS rhs
          when { lhs with min_len = 0 } = { rhs with min_len = 0 } ->
            Some
              (Formula.NLS { lhs with min_len = min lhs.min_len rhs.min_len })
        | _ -> None
      in
      Option.map (fun atom -> atom :: rest) new_atom
  | _ -> None

let remove_ptos_from_vars (vars : Formula.var list) (formula : Formula.t) =
  List.fold_left
    (fun formula var ->
      let atom = Formula.get_spatial_atom_from var formula in
      formula |> Formula.make_var_explicit_src var |> Formula.remove_atom atom)
    formula vars

(** Removes lists where the start and the end are syntactically equivalent, adds
    additional equalities that are known to be true *)
let remove_empty_lists (formula : Formula.t) : Formula.t =
  List.fold_left
    (fun formula -> function
      | Formula.LS ls when Formula.is_eq ls.first ls.next formula ->
          Formula.remove_atom (LS ls) formula
      | Formula.DLS dls
        when Formula.is_eq dls.first dls.next formula
             || Formula.is_eq dls.first dls.prev formula
             || Formula.is_eq dls.last dls.next formula
             || Formula.is_eq dls.last dls.prev formula ->
          formula
          |> Formula.remove_atom (DLS dls)
          |> Formula.add_eq dls.first dls.next
          |> Formula.add_eq dls.last dls.prev
      | Formula.NLS nls when Formula.is_eq nls.first nls.top formula ->
          Formula.remove_atom (NLS nls) formula
      | _ -> formula)
    formula formula

module Tests = struct
  open Testing
  open Formula

  let%test "remove_irrelevant_vars" =
    let f = [ Distinct (nil, x') ] |> remove_irrelevant_vars in
    assert_eq f []

  let%test "remove_irrelevant_vars_2" =
    let f = [ Freed x'; Distinct (nil, x') ] |> remove_irrelevant_vars in
    assert_eq f []
end
