
open Libsail

open Ast
open Ast_defs
open Ast_util

let invalid_decode str = prerr_endline ("invalid decode " ^ str); exit 1

let string_of_arg = function
  | E_aux (E_id id, _) -> "\"" ^ string_of_id id ^ "\""
  | exp -> invalid_decode ("call arg " ^ string_of_exp exp)
             
let rec get_calls exp = match exp with
  | E_aux (E_block exps, _) ->
     get_calls (Util.last exps)
  | E_aux (E_var (_, _, exp), _) ->
     get_calls exp
  | E_aux (E_app (f, args), _) ->
     print_endline ("call = \"" ^ string_of_id f ^ "\"");
     print_endline ("args = [" ^ Util.string_of_list ", " string_of_arg args ^ "]")
  | _ -> invalid_decode ("calls " ^ string_of_exp exp)

let bits = function
  | Typ_aux (Typ_app (id, [A_aux (A_nexp (Nexp_aux (Nexp_constant n, _)), _)]), _) ->
     Big_int.to_int n
  | typ -> invalid_decode ("bits " ^ string_of_typ typ)

let slice_info = function
  | E_aux (E_app (f, [E_aux (E_id op, _); hi; lo]), _)
       when string_of_id op = "op_code" && string_of_id f = "subrange_bits" ->
     string_of_exp hi ^ ", " ^ string_of_exp lo
  | E_aux (E_vector [E_aux (E_app (f, [E_aux (E_id op, _); bit]), _)], _)
       when string_of_id op = "op_code" && string_of_id f = "bitvector_access" ->
     string_of_exp bit
  | exp -> invalid_decode ("slice_info " ^ string_of_exp exp)
         
let rec get_slice first exp = match exp with
  | E_aux (E_block exps, _) ->
     List.fold_left get_slice first exps
  | E_aux (E_var (LEXP_aux (LEXP_cast (typ, id), _), slice, exp), _) ->
     if not first then (
       print_string ", "
     );
     print_string ("\"" ^ string_of_id id ^ "\" = [" ^ slice_info slice ^ "]");
     get_slice false exp
  | _ ->
     first
 
let get_see exp = match exp with
  | E_aux (E_block (E_aux (E_assign (LEXP_aux (LEXP_id id, _), number), _) :: _), _) when string_of_id id = "SEE" ->
     print_endline ("see = " ^ string_of_exp number)
  | _ -> invalid_decode ("SEE " ^ string_of_exp exp)

let to_hex str =
  if not (String.length str mod 4 = 0) then
    invalid_decode ("to_hex " ^ str)
  else
    let rec to_hex' n str =
      if n <= String.length str - 4 then (
        let digit = match String.sub str n 4 with
          | "0000" -> "0"
          | "0001" -> "1"
          | "0010" -> "2"
          | "0011" -> "3"
          | "0100" -> "4"
          | "0101" -> "5"
          | "0110" -> "6"
          | "0111" -> "7"
          | "1000" -> "8"
          | "1001" -> "9"
          | "1010" -> "a"
          | "1011" -> "b"
          | "1100" -> "c"
          | "1101" -> "d"
          | "1110" -> "e"
          | "1111" -> "f"
          | _ -> failwith "invalid nibble" in
        digit ^ to_hex' (n + 4) str
      ) else (
        ""
      )
    in
    to_hex' 0 str

let rec get_mask (P_aux (aux, _) as pat) =
  let get_mask_part mask (P_aux (aux, _) as pat) =
    match aux with
    | P_typ (typ, P_aux (P_wild, _)) ->
       String.make (bits typ) '0'
    | P_lit (L_aux (L_bin str, _)) ->
       if mask then (
         String.make (String.length str) '1'
       ) else (
         str
       )
    | _ -> invalid_decode ("get_mask_part " ^ string_of_pat pat)
  in
  match aux with
  | P_vector_concat pats ->
     print_endline ("bits = \"" ^ to_hex (Util.string_of_list "" (get_mask_part false) pats) ^ "\"");
     print_endline ("mask = \"" ^ to_hex (Util.string_of_list "" (get_mask_part true) pats) ^ "\"")
  | P_lit _ ->
     print_endline ("bits = \"" ^ to_hex (get_mask_part false pat) ^ "\"");
     print_endline ("mask = \"" ^ to_hex (get_mask_part true pat) ^ "\"")
  | P_as (pat, id) when string_of_id id = "op_code" ->
     get_mask pat
  | _ -> invalid_decode ("get_mask" ^ string_of_pat pat)

let arm_decode_info ast env =
  List.iter (fun def ->
      match def with
      | DEF_scattered (SD_aux (SD_funcl (FCL_aux (FCL_Funcl (id, pexp), _)), _)) when Id.compare id (mk_id "decode64") = 0 ->
         begin match pexp with
         | (Pat_aux (Pat_when (pat, _, exp), _) | Pat_aux (Pat_exp (pat, exp), _)) ->
            print_endline "[[opcode]]";
            get_calls exp;
            get_mask pat;
            print_string "slice = { ";
            let _ = get_slice true exp in
            print_endline " }";
            get_see exp;
            print_string "\n"
         end
      | _ -> ()
    ) ast.defs;
  exit 0
  
let _ =
  Target.register
    ~name:"arm_decode"
    ~pre_descatter_hook:arm_decode_info
    (fun _ _ _ _ _ -> ())
