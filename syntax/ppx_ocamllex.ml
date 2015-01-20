(* The package sedlex is released under the terms of an MIT-like license. *)
(* See the attached LICENSE file.                                         *)
(* The original version of this file was based on ppx_sedlex.ml,          *)
(* Copyright 2005, 2013 by Alain Frisch and LexiFi.                       *)

open Longident
open Parsetree
open Asttypes
open Ast_helper
open Ast_convenience
open Syntax
open Lexgen

let utf8_mode = ref false

let utf8_set cs =
  let rg a b = Characters [a, b] in
  let aux (a, b) =
    let a = min a b and b = max a b in
    let lo x i = 0x80 lor ((x lsr (6 * i)) land 0x3f) in
    let r = ref [] in
    if a <= 0x7f then begin
      let b = min b 0x7f in
      r := [rg a b]
    end;
    if a <= 0x7ff then begin
      let b = min b 0x7ff in
      let hi x = 0xc0 lor ((x lsr 6) land 0x1f) in
      let a0 = lo a 0 and a1 = hi a in
      let b0 = lo b 0 and b1 = hi b in
      r := Sequence (rg a1 b1, rg a0 b0) :: !r
    end;
    if a <= 0xffff then begin
      let b = min b 0xffff in
      let hi x = 0xe0 lor ((x lsr 12) land 0xf) in
      let a0 = lo a 0 and a1 = lo a 1 and a2 = hi a in
      let b0 = lo b 0 and b1 = lo b 1 and b2 = hi b in
      r := Sequence (rg a2 b2, Sequence (rg a1 b1, rg a0 b0)) :: !r
    end;
    if a <= 0x1fffff then begin
      let b = min b 0x1fffff in
      let hi x = 0xf0 lor ((x lsr 16) land 0x7) in
      let a0 = lo a 0 and a1 = lo a 1 and a2 = lo a 2 and a3 = hi a in
      let b0 = lo b 0 and b1 = lo b 1 and b2 = lo b 2 and b3 = hi b in
      r := Sequence (rg a3 b3, Sequence (rg a2 b2, Sequence (rg a1 b1, rg a0 b0))) :: !r
    end;
    !r
  in
  let rec alt = function
    | [] -> Eof | [r] -> r
    | r :: rl -> Alternative (r, alt rl)
  in
  alt (List.concat (List.map aux cs))

let rec remove_utf8 = function
  | Epsilon -> Epsilon
  | Characters cs -> utf8_set cs
  | Eof -> Eof
  | Sequence (r1, r2) -> Sequence (remove_utf8 r1, remove_utf8 r2)
  | Alternative (r1, r2) -> Alternative (remove_utf8 r1, remove_utf8 r2)
  | Repetition r -> Repetition (remove_utf8 r)
  | Bind (r, x) -> Bind (remove_utf8 r, x)

(* Helpers to build AST *)

let appfun s l = app (evar s) l
let pint i = Pat.constant (Const_int i)
let glb_value name def = Str.value Nonrecursive [Vb.mk (pvar name) def]
let sequence = function
  | [] -> unit ()
  | hd :: tl -> List.fold_left (fun e1 e2 -> Exp.sequence e1 e2) hd tl

(* Named regexps *)

module StringMap = Map.Make(struct
  type t = string
  let compare = compare
end)

let builtin_regexps =
  List.fold_left (fun acc (n, c) -> StringMap.add n (Characters c) acc)
    StringMap.empty
    [
     (* "any", Cset.any; *)
     (* "eof", Cset.eof; *)
     (* "xml_letter", Cset.letter; *)
     (* "xml_digit", Cset.digit; *)
     (* "xml_extender", Cset.extender; *)
     (* "xml_base_char", Cset.base_char; *)
     (* "xml_ideographic", Cset.ideographic; *)
     (* "xml_combining_char", Cset.combining_char; *)
     (* "xml_blank", Cset.blank; *)
     (* "tr8876_ident_char", Cset.tr8876_ident_char; *)

     (* Unicode 6.3 categories *)
     "cc", Unicode63.Categories.cc;
     "cf", Unicode63.Categories.cf;
     "cn", Unicode63.Categories.cn;
     "co", Unicode63.Categories.co;
     "cs", Unicode63.Categories.cs;
     "ll", Unicode63.Categories.ll;
     "lm", Unicode63.Categories.lm;
     "lo", Unicode63.Categories.lo;
     "lt", Unicode63.Categories.lt;
     "lu", Unicode63.Categories.lu;
     "mc", Unicode63.Categories.mc;
     "me", Unicode63.Categories.me;
     "mn", Unicode63.Categories.mn;
     "nd", Unicode63.Categories.nd;
     "nl", Unicode63.Categories.nl;
     "no", Unicode63.Categories.no;
     "pc", Unicode63.Categories.pc;
     "pd", Unicode63.Categories.pd;
     "pe", Unicode63.Categories.pe;
     "pf", Unicode63.Categories.pf;
     "pi", Unicode63.Categories.pi;
     "po", Unicode63.Categories.po;
     "ps", Unicode63.Categories.ps;
     "sc", Unicode63.Categories.sc;
     "sk", Unicode63.Categories.sk;
     "sm", Unicode63.Categories.sm;
     "so", Unicode63.Categories.so;
     "zl", Unicode63.Categories.zl;
     "zp", Unicode63.Categories.zp;
     "zs", Unicode63.Categories.zs;

     (* Unicode 6.3 properties *)
     "alphabetic", Unicode63.Properties.alphabetic;
     "ascii_hex_digit", Unicode63.Properties.ascii_hex_digit;
     "hex_digit", Unicode63.Properties.hex_digit;
     "id_continue", Unicode63.Properties.id_continue;
     "id_start", Unicode63.Properties.id_start;
     "lowercase", Unicode63.Properties.lowercase;
     "math", Unicode63.Properties.math;
     "other_alphabetic", Unicode63.Properties.other_alphabetic;
     "other_lowercase", Unicode63.Properties.other_lowercase;
     "other_math", Unicode63.Properties.other_math;
     "other_uppercase", Unicode63.Properties.other_uppercase;
     "uppercase", Unicode63.Properties.uppercase;
     "white_space", Unicode63.Properties.white_space;
     "xid_continue", Unicode63.Properties.xid_continue;
     "xid_start", Unicode63.Properties.xid_start;
    ]

let array_set a i e =
  app (evar "Array.set") [a; i; e]

let array_get a i =
  app (evar "Array.get") [a; i]

let lex_mem lexbuf =
  Exp.field (evar lexbuf) (lid "Lexing.lex_mem")

let gen_mem_access lexbuf i =
  array_get (lex_mem lexbuf) (int i)

let gen_mem_action lexbuf = function
  | Copy (tgt, src) ->
      array_set (lex_mem lexbuf) (int tgt) (array_get (lex_mem lexbuf) (int src))
  | Set tgt ->
      array_set (lex_mem lexbuf) (int tgt) (Exp.field (evar lexbuf) (lid "Lexing.lex_curr_pos"))

let gen_action (lexbuf : string) mems r =
  let action = match r with
    | Backtrack ->
        [ Exp.setfield (evar lexbuf) (lid "Lexing.lex_curr_pos") (Exp.field (evar lexbuf) (lid "Lexing.lex_last_pos"));
          Exp.field (evar lexbuf) (lid "Lexing.lex_last_action") ]
    | Goto n ->
        [app (evar (Printf.sprintf "__ocaml_lex_state%d" n)) [evar lexbuf]]
  in
  sequence ((List.map (gen_mem_action lexbuf) mems) @ action)

let rec gen_pats = function
  | [] -> assert false
  | [n] -> pint n
  | n :: pats -> Pat.or_ (pint n) (gen_pats pats)

let gen_default_clause lexbuf mems r =
  Exp.case (Pat.any ()) (gen_action lexbuf mems r)

let gen_moves lexbuf moves =
  Printf.eprintf "GEN_MOVES: Array.length moves = %d\n%!" (Array.length moves);
  let t = Hashtbl.create 17 in
  let add_move i (m,mems) =
    let mems,r = try Hashtbl.find t m with Not_found -> mems,[] in
    Hashtbl.replace t m (mems,(i::r))
  in
  for i = 0 to 256 do add_move i moves.(i) done;
  let most_frequent = ref Backtrack
  and most_mems = ref []
  and size = ref 0 in
  Hashtbl.iter
    (fun m (mems, pats) ->
      let size_m = List.length pats in
      if size_m > !size then begin
        most_frequent := m ;
        most_mems := mems ;
        size := size_m
      end)
    t;
  Printf.eprintf "NOTE: Hashtbl.length = %d\n%!" (Hashtbl.length t);
  Hashtbl.fold
    (fun m (mems, pats) rem ->
       if m <> !most_frequent then
         Exp.case (gen_pats (List.rev pats)) (gen_action lexbuf mems m) :: rem else rem)
    t [gen_default_clause lexbuf !most_mems !most_frequent]

let gen_tag_actions lexbuf mvs =
  List.map
    (function
      | SetTag (t, m) ->
          array_set (lex_mem lexbuf) (int t) (array_get (lex_mem lexbuf) (int m))
      | EraseTag t ->
          array_set (lex_mem lexbuf) (int t) (int (-1))) mvs

let gen_trans lexbuf i trans =
  let entry = Printf.sprintf "__ocaml_lex_state%d" i in
  let body = match trans with
    | Perform (n, mvs) ->
        sequence (gen_tag_actions lexbuf mvs @ [int n])
    | Shift (trans, moves) ->
        let m =
          Exp.match_ (app (evar "Lexing_plus.next_char") [evar lexbuf])
            (gen_moves lexbuf moves)
        in
        begin match trans with
        | Remember (n, mvs) ->
            sequence (gen_tag_actions lexbuf mvs @
                      [Exp.setfield (evar lexbuf) (lid "Lexing.lex_last_pos") (Exp.field (evar lexbuf) (lid "Lexing.lex_curr_pos"));
                       Exp.setfield (evar lexbuf) (lid "Lexing.lex_last_action") (int n);
                       m])
        | No_remember -> m
        end
  in
  Vb.mk (pvar entry) (lam (pvar lexbuf) body)

let gen_automata lexbuf auto entry =
  let_in ~recursive:true (Array.to_list (Array.mapi (gen_trans lexbuf) auto)) entry

let gen_base_mem lexbuf = function
  | Mem i -> gen_mem_access lexbuf i
  | Start -> Exp.field (evar lexbuf) (lid "Lexing.lex_start_pos")
  | End -> Exp.field (evar lexbuf) (lid "Lexing.lex_curr_pos")

let gen_tag_access lexbuf = function
  | Sum (a, 0) ->
      gen_base_mem lexbuf a
  | Sum (a, i) ->
      app (evar "+") [gen_base_mem lexbuf a; int i]

let gen_env lexbuf env act =
  match env with
  | [] -> act
  | _  ->
      (* Probably, we are better with variables sorted
         in apparition order *)
      let env =
        List.sort
          (fun ((_,p1),_) ((_,p2),_) ->
            Pervasives.compare p1.start_pos  p2.start_pos)
          env in

      let_in ~recursive:false
        (List.map (fun ((x, pos), v) ->
             Vb.mk (pvar x)
               begin match v with
               | Ident_string (true, nstart, nend) ->
                   app (evar "Lexing.sub_lexeme_opt") [evar lexbuf; gen_tag_access lexbuf nstart; gen_tag_access lexbuf nend]
               | Ident_string (false, nstart, nend) ->
                   app (evar "Lexing.sub_lexeme") [evar lexbuf; gen_tag_access lexbuf nstart; gen_tag_access lexbuf nend]
               | Ident_char (true, nstart) ->
                   app (evar "Lexing.sub_lexeme_char_opt") [evar lexbuf; gen_tag_access lexbuf nstart]
               | Ident_char (false, nstart) ->
                   app (evar "Lexing.sub_lexeme_char") [evar lexbuf; gen_tag_access lexbuf nstart]
               end) env)
        act

let gen_definition lexbuf e transitions (* error *) =
  let init_num, init_moves = e.auto_initial_state in
  let entry =
    sequence
      (app (evar "Lexing_plus.init_lexbuf") [evar lexbuf; int e.auto_mem_size] ::
       (List.map (gen_mem_action lexbuf) init_moves) @
       [let_in ~recursive:false
          [Vb.mk (pvar "__ocaml_lex_result") (app (evar (Printf.sprintf "__ocaml_lex_state%d" init_num)) [evar lexbuf])]
          (sequence
             [ Exp.setfield (evar lexbuf) (lid "Lexing.lex_start_p")
                 (Exp.field (evar lexbuf) (lid "Lexing.lex_curr_p"));
               Exp.setfield (evar lexbuf) (lid "Lexing.lex_curr_p")
                 (Exp.record [lid "Lexing.pos_cnum",
                              (app (evar "+") [Exp.field (evar lexbuf) (lid "Lexing.lex_abs_pos");
                                               Exp.field (evar lexbuf) (lid "Lexing.lex_curr_pos")])]
                    (Some (Exp.field (evar lexbuf) (lid "Lexing.lex_curr_p"))));
               Exp.match_ (evar "__ocaml_lex_result")
                 ((List.map (fun (num, env, act) ->
                      Exp.case (pint num) (gen_env lexbuf env act)) e.auto_actions) @
                  [Exp.case (Pat.any ()) (app (evar "raise") [constr "Failure" [str "lexing: empty token"]])]
                 ) ])])
  in
  gen_automata lexbuf transitions entry

(* Lexer specification parser *)

let codepoint i =
  if i < 0 || (i > 0x10FFFF && !utf8_mode) || (i > 255 && not !utf8_mode) then
    failwith (Printf.sprintf "Invalid character code: %i" i);
  i

let regexp_for_char c =
  Characters (Cset.singleton (Char.code c))

let regexp_for_string s =
  let rec aux n =
    if n = String.length s then Epsilon
    else
      Sequence (regexp_for_char s.[n], aux (succ n))
  in aux 0

let err loc s =
  raise (Location.Error (Location.error ~loc ("Ocamllex: " ^ s)))

let as_cset = function
  | Characters s -> Some s
  | _ -> None

let regexp_of_pattern env =
  let rec aux p =
    match p.ppat_desc with
    | Ppat_or (p1, p2) -> Alternative (aux p1, aux p2)
    | Ppat_tuple (p :: pl) ->
        List.fold_left (fun r p -> Sequence (r, aux p))
          (aux p)
          pl
    | Ppat_construct ({txt = Lident "Star"}, Some p) ->
        Repetition (aux p)
    | Ppat_construct ({txt = Lident "Plus"}, Some p) ->
        let r = aux p in
        Sequence (r, Repetition r)
    | Ppat_construct ({txt = Lident "Opt"}, Some p) ->
        Alternative (Epsilon, aux p)
    | Ppat_construct ({txt = Lident "Eof"}, None) ->
        Eof
    | Ppat_any -> Characters [0, if !utf8_mode then 0x10FFFF else 255]
    | Ppat_construct ({txt = Lident "Diff"}, Some {ppat_desc = Ppat_tuple [p1; p2]}) ->
        begin match as_cset (aux p1), as_cset (aux p2) with
          | Some s1, Some s2 ->
              Characters (Cset.diff s1 s2)
          | None, _ ->
              err p1.ppat_loc
                "the Diff operator can only be applied to a pair of single-character regexps."
          | _, None ->
              err p2.ppat_loc
                "the Diff operator can only be applied to a pair of single-character regexps."
        end
    | Ppat_construct ({txt = Lident "Compl"}, Some p0) ->
        begin match as_cset (aux p0) with
          | Some s0 ->
              Characters (Cset.diff [0, if !utf8_mode then 0x10FFFF else 255] s0)
          | None ->
              err p0.ppat_loc
                "the Compl operator can only be applied to a single-characer regexp"
        end
    | Ppat_construct ({txt = Lident "Chars"}, Some {ppat_desc=Ppat_constant (Const_string (s, _))}) ->
        let c = ref Cset.empty in
        for i = 0 to String.length s - 1 do
          c := Cset.union !c (Cset.singleton (Char.code s.[i]))
        done;
        Characters !c
    | Ppat_interval (Const_char c1, Const_char c2) ->
        Characters [Char.code c1, Char.code c2]
    | Ppat_interval (Const_int i1, Const_int i2) ->
        Characters [codepoint i1, codepoint i2]

    | Ppat_constant (Const_string (s, _)) -> regexp_for_string s
    | Ppat_constant (Const_char c) -> regexp_for_char c
    | Ppat_constant (Const_int c) -> Characters (Cset.singleton (codepoint c))
    | Ppat_var {txt=x} ->
        begin try StringMap.find x env
        with Not_found ->
          err p.ppat_loc (Printf.sprintf "unbound regexp %s" x)
        end
    | Ppat_alias (p, v) ->
        let loc = { loc_file = ""; start_pos = 0; end_pos = 0; start_line = 0; start_col = 0 } in
        Bind (aux p, (v.txt, loc))
    | _ ->
      err p.ppat_loc "this pattern is not a valid regexp"
  in
  aux


let mapper =
  object(this)
    inherit Ast_mapper_class.mapper as super

    val env = builtin_regexps

    method define_regexp name p =
      {< env = StringMap.add name (regexp_of_pattern env p) env >}

    method! expr e =
      match e with
      | [%expr [%ocamllex [%e? {pexp_desc=Pexp_match (lexbuf, cases)}]]] ->
            let lexbuf =
              match lexbuf with
              | {pexp_desc=Pexp_ident{txt=Lident lexbuf}} -> lexbuf
              | _ ->
                err lexbuf.pexp_loc "the matched expression must be a single identifier"
            in
            let cases =
              List.map
                (function
                  | {pc_lhs = p; pc_rhs = e; pc_guard = None} -> regexp_of_pattern env p, super # expr e
                  | {pc_guard = Some e} ->
                    err e.pexp_loc "'when' guards are not supported"
                ) cases
            in
            let cases = if !utf8_mode then List.map (fun (r, e) -> (remove_utf8 r, e)) cases else cases in
            let lexdef = { name="dummyname"; args = (); shortest = false; clauses = cases } in
            let entry_points, transitions = make_dfa lexdef in
            gen_definition lexbuf entry_points transitions
      | [%expr let [%p? {ppat_desc=Ppat_var{txt=name}}] = [%sedlex.regexp? [%p? p]] in [%e? body]] ->
          (this # define_regexp name p) # expr body
      | [%expr [%sedlex [%e? _]]] ->
        err e.pexp_loc "the %ocamllex extension is only recognized on match expressions"
      | _ -> super # expr e


    val toplevel = true

    method structure_with_regexps l =
      let mapper = ref this in
      let regexps = ref [] in
      let l = List.concat
        (List.map
           (function
             | [%stri let [%p? {ppat_desc=Ppat_var{txt=name}}] = [%ocamllex.regexp? [%p? p]]] as i ->
               regexps := i :: !regexps;
               mapper := !mapper # define_regexp name p;
               []
             | i ->
               [ !mapper # structure_item i ]
         ) l) in
      (l, List.rev !regexps)

    method! structure l =
      if toplevel then
        let sub = {< toplevel = false >} in
        let previous =
          match Ast_mapper.get_cookie "ocamllex.regexps" with
          | Some {pexp_desc = Pexp_extension (_, PStr l)} -> l
          | Some _ -> assert false
          | None -> []
        in
        let l, regexps = sub # structure_with_regexps (previous @ l) in
        Ast_mapper.set_cookie "ocamllex.regexps" (Exp.extension (Location.mknoloc "regexps", PStr regexps));
        l
      else
        fst (this # structure_with_regexps l)

 end

let () =
  (* Utf8.utf8_mode := true; *)
  Ast_mapper.register "ocamllex" (fun _ -> Ast_mapper_class.to_mapper mapper)