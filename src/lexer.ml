open Types
open Str
(* Part 1: Lexer - IMPLEMENT YOUR CODE BELOW *)

let tokenize input = 
  let rec tok pos =
    if pos >= String.length input then []
    else if (Str.string_match (Str.regexp "[ \n\t]+") input pos) then 
      tok (Str.match_end())
    else if (Str.string_match (Str.regexp "[0-9]+") input pos) then 
      let num = int_of_string (Str.matched_string input) in
      let next_pos = Str.match_end () in
      (Tok_Int num) :: tok next_pos 
      else if (Str.string_match (Str.regexp "\"[^\"]*\"") input pos) then 
        let strng = Str.matched_string input in 
        (Tok_String(String.sub strng 1 (String.length strng - 2))) :: tok(Str.match_end())  
      else if Str.string_match (Str.regexp "true") input pos then
        (Tok_Bool true) :: tok (Str.match_end ())
    else if Str.string_match (Str.regexp "false") input pos then
        (Tok_Bool false) :: tok (Str.match_end ())   
    else if Str.string_match(Str.regexp "&&") input pos then 
        Tok_And :: tok(Str.match_end())
    else if Str.string_match(Str.regexp "||") input pos then 
        Tok_Or :: tok(Str.match_end())
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
        | _ -> Tok_ID id
      in
      token :: tok (Str.match_end())
    else if (Str.string_match (Str.regexp ";;") input pos) then 
      Tok_DoubleSemi :: tok (Str.match_end ()) 
    else if (Str.string_match (Str.regexp "->") input pos) then 
      Tok_Arrow :: tok (Str.match_end ())
    else if (Str.string_match (Str.regexp ";") input pos) then 
      Tok_Semi :: tok (Str.match_end ())
    else if (Str.string_match (Str.regexp "=") input pos) then 
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
    else if (Str.string_match (Str.regexp ".") input pos) then 
      Tok_Dot :: tok (Str.match_end ())
    else 
      raise (InvalidInputException "Wrong language")
  in
  tok 0
    
