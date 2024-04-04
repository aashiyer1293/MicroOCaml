open Types
open Str
(* Part 1: Lexer - IMPLEMENT YOUR CODE BELOW *)

let tokenize input = 
  let rec tok pos =
    if pos >= String.length input then []
    else if (Str.string_match (Str.regexp "[ \n\t]+") input pos) then 
      tok (Str.match_end())  
    else if (Str.string_match (Str.regexp "-?[0-9]+") input pos) then 
        (Tok_Int (int_of_string(Str.matched_string input))) :: tok(Str.match_end())
    else if (Str.string_match (Str.regexp "\"[^\"]*\"") input pos) then 
      let strng = Str.matched_string input in 
      (Tok_String(String.sub input 1 (String.length input - 2))) :: tok(Str.match_end())
    else if (Str.string_match (Str.regexp "true\\|false") input pos) then 
      (Tok_Bool (bool_of_string(Str.matched_string input))) :: tok(Str.match_end())
    else if (Str.string_match (Str.regexp "[a-zA-Z][a-zA-Z0-9]*") input pos) then 
      let id = Str.matched_string input in
      let token = 
        match id with 
        | "let" -> Tok_Let
        | "rec" -> Tok_Rec
        | "in" -> Tok_In
        | "def" -> Tok_Def
        | "fun" -> Tok_Fun
        | "if" -> Tok_If
        | "then" -> Tok_Then
        | "else" -> Tok_Else
        | "not" -> Tok_Not
        | "or" -> Tok_Or
        | "and" -> Tok_And
        | _ -> Tok_ID id
      in
      token :: tok (Str.match_end())
    else if (Str.string_match (Str.regexp ";") input pos) then 
      Tok_Semi :: tok (Str.match_end ())
    else if (Str.string_match (Str.regexp ";;") input pos) then 
      Tok_DoubleSemi :: tok (Str.match_end ())
    else if (Str.string_match (Str.regexp "==") input pos) then 
      Tok_Equal :: tok (Str.match_end ())
    else if (Str.string_match (Str.regexp "<>") input pos) then 
      Tok_NotEqual :: tok (Str.match_end ())
    else if (Str.string_match (Str.regexp "<=") input pos) then 
      Tok_LessEqual :: tok (Str.match_end ())
    else if (Str.string_match (Str.regexp ">=") input pos) then 
      Tok_GreaterEqual :: tok (Str.match_end ())
    else if (Str.string_match (Str.regexp "<") input pos) then 
      Tok_Less :: tok (Str.match_end ())
    else if (Str.string_match (Str.regexp ">") input pos) then 
      Tok_Greater :: tok (Str.match_end ())
    else if (Str.string_match (Str.regexp "\\+") input pos) then 
      Tok_Add :: tok (Str.match_end ())
    else if (Str.string_match (Str.regexp "-") input pos) then 
      Tok_Sub :: tok (Str.match_end ())
    else if (Str.string_match (Str.regexp "\\*") input pos) then 
      Tok_Mult :: tok (Str.match_end ())
    else if (Str.string_match (Str.regexp "/") input pos) then 
      Tok_Div :: tok (Str.match_end ())
    else if (Str.string_match (Str.regexp "->") input pos) then 
      Tok_Arrow :: tok (Str.match_end ())
    else if (Str.string_match (Str.regexp "\\^") input pos) then 
      Tok_Concat :: tok (Str.match_end ())
    else if (Str.string_match (Str.regexp "{") input pos) then 
      Tok_LCurly :: tok (Str.match_end ())
    else if (Str.string_match (Str.regexp "}") input pos) then 
      Tok_RCurly :: tok (Str.match_end ())
    else if (Str.string_match (Str.regexp "(") input pos) then 
      Tok_LParen :: tok (Str.match_end ())
    else if (Str.string_match (Str.regexp ")") input pos) then 
      Tok_RParen :: tok (Str.match_end ())
    else 
      raise (InvalidInputException "Wrong language")
  in
  tok 0
    
