    open Types
    open Utils

    (* Provided functions - DO NOT MODIFY *)

    (* Matches the next token in the list, throwing an error if it doesn't match the given token *)
    let match_token (toks : token list) (tok : token) =
      match toks with
      | [] -> raise (InvalidInputException (string_of_token tok))
      | h :: t when h = tok -> t
      | h :: _ ->
          raise
            (InvalidInputException
              (Printf.sprintf "Expected %s from input %s, got %s"
                  (string_of_token tok) 
                  (string_of_list string_of_token toks)
                  (string_of_token h)))

    (* Matches a sequence of tokens given as the second list in the order in which they appear, throwing an error if they don't match *)
    let match_many (toks : token list) (to_match : token list) =
      List.fold_left match_token toks to_match

    (* Return the next token in the token list as an option *)
    let lookahead (toks : token list) =
      match toks with [] -> None | h :: t -> Some h

    (* Return the token at the nth index in the token list as an option*)
    let rec lookahead_many (toks : token list) (n : int) =
      match (toks, n) with
      | h :: _, 0 -> Some h
      | _ :: t, n when n > 0 -> lookahead_many t (n - 1)
      | _ -> None

    (* Part 2: Parsing expressions *)

    let rec parse_expr tokens = 
      match lookahead tokens with
      | Some Tok_Let -> parse_let tokens
      | Some Tok_If -> parse_if tokens
      | Some Tok_Fun -> parse_func tokens
      | Some Tok_Int _ | Some Tok_Bool _ | Some Tok_String _ | Some Tok_ID _ | Some Tok_LParen | Some Tok_LCurly | Some Tok_Not  -> parse_or tokens
      | _ -> raise (InvalidInputException "Unexpected or no token")


      and parse_let tokens = 
        match lookahead tokens with 
        | Some Tok_Let ->
            let tokens = match_token tokens Tok_Let in 
            let (is_rec, tokens) = (match lookahead tokens with 
            | Some Tok_Rec -> (true, match_token tokens Tok_Rec)
            | _ -> (false, tokens))
            in
            (match lookahead tokens with 
              | Some Tok_ID id -> 
                let tokens = match_token tokens (Tok_ID id) in 
                let tokens = match_token tokens Tok_Equal in 
                let (tokens, e) = parse_expr tokens in
                let tokens = match_token tokens Tok_In in
                let (tokens, e2) = parse_expr tokens in 
                (tokens, Let(id, is_rec, e, e2))
              | _ -> raise (InvalidInputException "Why was there no ID"))
        | _ -> raise (InvalidInputException "Unexpected or no let")


        and parse_func tokens =
          match lookahead tokens with
          | Some Tok_Fun ->
              let tokens = match_token tokens Tok_Fun in 
              (match lookahead tokens with 
              | Some (Tok_ID id) ->
                  let tokens = match_token tokens (Tok_ID id) in
                  let tokens = match_token tokens Tok_Arrow in
                  let (rest, e) = parse_expr tokens in
                  (rest, Fun(id, e))
              | _ -> raise (InvalidInputException "Expected an ID after 'fun'"))
          | _ -> raise (InvalidInputException "Function expression must start with 'fun'")
        

        and parse_if tokens = 
          match lookahead tokens with 
          | Some Tok_If ->
            let tokens = match_token tokens Tok_If in
            let (tokens, e) = parse_expr tokens in 
            let tokens = match_token tokens Tok_Then in
            let (tokens, e1) = parse_expr tokens in 
            let tokens = match_token tokens Tok_Else in
            let (tokens, e2) = parse_expr tokens in 
            (tokens, If(e,e1,e2))
          | _ -> raise (InvalidInputException "Where's the IF")


        and parse_or tokens = 
          let (rest, e) = parse_and tokens in
            match lookahead rest with
              | Some Tok_Or ->
                let rest_or = match_token rest Tok_Or in
                let (rest1, e1) = parse_or rest_or in
                (rest1, Binop (Or, e, e1))
              | _ -> (rest, e)


        and parse_and tokens = 
          let (rest, e) = parse_equality tokens in
            match lookahead rest with
              | Some Tok_And ->
                let rest_and = match_token rest Tok_And in
                let (rest1, e1) = parse_and rest_and in
                (rest1, Binop (And, e, e1))
              | _ -> (rest, e)


        and parse_equality tokens = 
          let (rest, e) = parse_relational tokens in 
            match lookahead rest with
            | Some Tok_Equal ->
              let tokens = match_token rest Tok_Equal in 
              let (rest1, e1) = parse_equality tokens in 
              (rest1, Binop(Equal, e, e1))
            | Some Tok_NotEqual -> 
              let tokens = match_token rest Tok_NotEqual in 
              let (rest1, e1) = parse_equality tokens in 
              (rest1, Binop(NotEqual, e, e1)) 
            | _ -> (rest, e)

          
        and parse_relational tokens = 
            let (rest, e) = parse_additive tokens in 
              match lookahead rest with
                | Some Tok_Less ->
                  let tokens = match_token rest Tok_Less in 
                  let (rest1, e1) = parse_relational tokens in 
                  (rest1, Binop(Less, e, e1)) 
                | Some Tok_LessEqual -> 
                  let tokens = match_token rest Tok_LessEqual in 
                  let (rest1, e1) = parse_relational tokens in 
                  (rest1, Binop(LessEqual, e, e1))
                | Some Tok_Greater ->
                  let tokens = match_token rest Tok_Greater in 
                  let (rest1, e1) = parse_relational tokens in 
                  (rest1, Binop(Greater, e, e1))
                | Some Tok_GreaterEqual -> 
                  let tokens = match_token rest Tok_GreaterEqual in 
                  let (rest1, e1) = parse_relational tokens in 
                  (rest1, Binop(GreaterEqual, e, e1))
                | _ -> (rest, e)

        and parse_additive tokens = 
          let (tokens, e) = parse_mult tokens in
            match lookahead tokens with 
              | Some Tok_Add ->
                let tokens = match_token tokens Tok_Add in
                let (rest, e1) = parse_additive tokens in 
              (rest, Binop(Add, e, e1))
              | Some Tok_Sub -> 
                let tokens = match_token tokens Tok_Sub in
                let (rest, e1) = parse_additive tokens in 
                (rest, Binop(Sub, e, e1))
              | _ -> (tokens, e)
        
        
          and parse_mult tokens =
            let (tokens, e) = parse_concat tokens in
            match lookahead tokens with 
            | Some Tok_Mult ->
              let tokens = match_token tokens Tok_Mult in
              let (rest, e1) = parse_mult tokens in 
              (rest, Binop(Mult, e, e1))
            | Some Tok_Div -> 
              let tokens = match_token tokens Tok_Div in
              let (rest, e1) = parse_mult tokens in 
              (rest, Binop(Div, e, e1))
            | _ -> (tokens, e)

          and parse_concat tokens = 
            let (tokens, e) = parse_unary tokens in
            match lookahead tokens with 
            | Some Tok_Concat -> 
              let tokens = match_token tokens Tok_Concat in
              let (rest, e1) = parse_concat tokens in 
              (rest, Binop(Concat, e, e1))
            | _ -> (tokens, e)


          and parse_unary tokens = 
            match lookahead tokens with 
            | Some Tok_Not ->
              let tokens = match_token tokens Tok_Not in
              let (rest, e) = parse_unary tokens in 
              (rest, Not e)
            | _ -> parse_app tokens
          
          and parse_app tokens = 
            let (tokens, e) = parse_select tokens in
            match lookahead tokens with 
            | Some (Tok_Int _ | Tok_Bool _ | Tok_String _ | Tok_ID _ | Tok_LParen) ->
              let (rest, e1) = parse_prim tokens in 
              (rest, App(e, e1))
            | _ -> (tokens, e)

          and parse_select tokens =
            let (tokens, e) = parse_prim tokens in
              match lookahead tokens with 
              | Some Tok_Dot -> 
                let tokens = match_token tokens Tok_Dot in
                (match lookahead tokens with 
                | Some (Tok_ID id) ->
                  let tokens = match_token tokens (Tok_ID id) in 
                  (tokens, Select(Lab id, e))
                | _ -> raise (InvalidInputException "No ID"))
              | _ -> (tokens, e)

          
          and parse_prim tokens = 
            match tokens with 
            | Tok_Int i :: t -> (t, Int i)
            | Tok_Bool b :: t -> (t, Bool b)
            | Tok_String s :: t -> (t, String s)
            | Tok_ID id :: t -> (t, ID id)
            | Tok_LParen :: t -> 
              let (rest, e) = parse_expr t in
              let rest = match_token rest Tok_RParen in
              (rest, e)
          | _ -> parse_record tokens


          and parse_record tokens =
            match lookahead tokens with
            | Some Tok_LCurly -> 
              let tokens = match_token tokens Tok_LCurly in
              (match lookahead tokens with 
              | Some Tok_RCurly -> 
                let tokens = match_token tokens Tok_RCurly in  
                (tokens, Record [])
              | _ -> 
                let (tokens, e) = parse_record_body tokens in 
                let tokens = match_token tokens Tok_RCurly in  
                (tokens, Record e))
            | _ -> raise (InvalidInputException "No Opening Curly")
          
          and parse_record_body tokens = 
            match tokens with 
            | Tok_ID id :: Tok_Equal :: tokens -> 
              let (tokens, e) = parse_expr tokens in 
              (match lookahead tokens with 
              | Some Tok_Semi ->
                let tokens = match_token tokens Tok_Semi in
                let (tokens, e1) = parse_record_body tokens in
                (tokens, (Lab id, e) :: e1)
              | _ -> (tokens, [(Lab id, e)] ))
            | _ -> (tokens, [])


    (* Part 3: Parsing mutop *)

    let rec parse_mutop tokens = 
      match lookahead tokens with 
      | Some Tok_Def -> parse_def tokens
      | Some Tok_DoubleSemi -> ([], NoOp)
      | _ -> parse_exp_mutop tokens
    
    and parse_def tokens = 
      match lookahead tokens with 
      | Some Tok_Def -> 
        let tokens = match_token tokens Tok_Def in 
        (match lookahead tokens with 
        | Some Tok_ID id -> 
          let tokens = match_token tokens (Tok_ID id) in 
          let tokens = match_token tokens Tok_Equal in 
          let (tokens, e) = parse_expr tokens in
          let tokens = match_token tokens Tok_DoubleSemi in 
          (tokens, Def(id,e))
        | _ -> raise (InvalidInputException "No ID"))
      | _ -> raise (InvalidInputException "No def")


    and parse_exp_mutop tokens = 
      let (tokens, e) = parse_expr tokens in 
      let tokens = match_token tokens Tok_DoubleSemi in
      (tokens, Expr e)
