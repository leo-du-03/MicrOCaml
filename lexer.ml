open Types

(* Part 1: Lexer - IMPLEMENT YOUR CODE BELOW *)

let rec tokenize input = 
  let len = String.length input in

  let bool = Re.compile (Re.Perl.re "^(true|false)\\s*(.*)") in
  let numre = Re.compile (Re.Perl.re "^([0-9]+|\\(-[0-9]+\\))\\s*(.*)") in
  let neg = Re.compile (Re.Perl.re "^\\((-[0-9]+)\\)") in
  let string = Re.compile (Re.Perl.re "^\\\"([^\\\"]*)\\\"\\s*(.*)") in
  let id = Re.compile (Re.Perl.re "^([a-zA-Z][a-zA-Z0-9]*)\\s*(.*)") in
  let lpar = Re.compile (Re.Perl.re "^\\(\\s*(.*)") in
  let rpar = Re.compile (Re.Perl.re "^\\)\\s*(.*)") in
  let lcur = Re.compile (Re.Perl.re "^\\{\\s*(.*)") in
  let rcur = Re.compile (Re.Perl.re "^\\}\\s*(.*)") in
  let dot = Re.compile (Re.Perl.re "^\\.\\s*(.*)") in
  let eq = Re.compile (Re.Perl.re "^=\\s*(.*)") in
  let neq = Re.compile (Re.Perl.re "^<>\\s*(.*)") in
  let gt = Re.compile (Re.Perl.re "^>\\s*(.*)") in
  let lt = Re.compile (Re.Perl.re "^<\\s*(.*)") in
  let gte = Re.compile (Re.Perl.re "^>=\\s*(.*)") in
  let lte = Re.compile (Re.Perl.re "^<=\\s*(.*)") in
  let l_or = Re.compile (Re.Perl.re "^\\|\\|\\s*(.*)") in
  let l_and = Re.compile (Re.Perl.re "^\\&\\&\\s*(.*)") in
  let l_not = Re.compile (Re.Perl.re "^not\\s*(.*)") in
  let l_if = Re.compile (Re.Perl.re "^if\\s*(.*)") in
  let l_then = Re.compile (Re.Perl.re "^then\\s*(.*)") in
  let l_else = Re.compile (Re.Perl.re "^else\\s*(.*)") in
  let add = Re.compile (Re.Perl.re "^\\+\\s*(.*)") in
  let sub = Re.compile (Re.Perl.re "^\\-\\s*(.*)") in
  let mul = Re.compile (Re.Perl.re "^\\*\\s*(.*)") in
  let div = Re.compile (Re.Perl.re "^\\/\\s*(.*)") in
  let conc = Re.compile (Re.Perl.re "^\\^\\s*(.*)") in
  let l_let = Re.compile (Re.Perl.re "^let\\s*(.*)") in
  let l_def = Re.compile (Re.Perl.re "^def\\s*(.*)") in
  let l_in = Re.compile (Re.Perl.re "^in\\s*(.*)") in
  let l_rec = Re.compile (Re.Perl.re "^rec\\s*(.*)") in
  let l_fun = Re.compile (Re.Perl.re "^fun\\s*(.*)") in
  let arr = Re.compile (Re.Perl.re "^\\->\\s*(.*)") in
  let dbl_sm = Re.compile (Re.Perl.re "^;;\\s*(.*)") in
  let semi = Re.compile (Re.Perl.re "^;\\s*(.*)") in

  if input = "" then [] 
  else if Re.execp bool input then (* Bool *)
    let group = Re.exec bool input in
    let in_bool = Re.Group.get group 1 in
    let rest = Re.Group.get group 2 in
    let x = if in_bool = "true" then true else false in
    Tok_Bool(x)::(tokenize rest)
  else if Re.execp numre input then (* ints *)
    let group = Re.exec numre input in
    let in_int = Re.Group.get group 1 in
    let rest = Re.Group.get group 2 in
    let match_neg = Re.execp neg in_int in
    if match_neg then
      let group2 = Re.exec neg in_int in
      let neg_int = Re.Group.get group2 1 in
      Tok_Int(int_of_string neg_int)::(tokenize rest)
    else
      Tok_Int(int_of_string in_int)::(tokenize rest)
  else if Re.execp string input then (* strings *)
    let group = Re.exec string input in
    let in_str = Re.Group.get group 1 in
    let rest = Re.Group.get group 2 in
    Tok_String(in_str)::(tokenize rest)
  else if Re.execp lpar input then (* LParens *)
    let group = Re.exec lpar input in
    let rest = Re.Group.get group 1 in
    Tok_LParen::(tokenize rest)
  else if Re.execp rpar input then (* RParens *)
    let group = Re.exec rpar input in
    let rest = Re.Group.get group 1 in
    Tok_RParen::(tokenize rest)
  else if Re.execp lcur input then (* LCurly *)
    let group = Re.exec lcur input in
    let rest = Re.Group.get group 1 in
    Tok_LCurly::(tokenize rest)
  else if Re.execp rcur input then (* RCurly *)
    let group = Re.exec rcur input in
    let rest = Re.Group.get group 1 in
    Tok_RCurly::(tokenize rest)
  else if Re.execp arr input then (* Arrow *)
    let group = Re.exec arr input in
    let rest = Re.Group.get group 1 in
    Tok_Arrow::(tokenize rest)
  else if Re.execp dot input then (* Dot *)
    let group = Re.exec dot input in
    let rest = Re.Group.get group 1 in
    Tok_Dot::(tokenize rest)
  else if Re.execp eq input then (* Equal *)
    let group = Re.exec eq input in
    let rest = Re.Group.get group 1 in
    Tok_Equal::(tokenize rest)
  else if Re.execp neq input then (* Not Equal *)
    let group = Re.exec neq input in
    let rest = Re.Group.get group 1 in
    Tok_NotEqual::(tokenize rest)
  else if Re.execp gte input then (* Greater Equal *)
    let group = Re.exec gte input in
    let rest = Re.Group.get group 1 in
    Tok_GreaterEqual::(tokenize rest)
  else if Re.execp lte input then (* Less Equal *)
    let group = Re.exec lte input in
    let rest = Re.Group.get group 1 in
    Tok_LessEqual::(tokenize rest)
  else if Re.execp gt input then (* Greater *)
    let group = Re.exec gt input in
    let rest = Re.Group.get group 1 in
    Tok_Greater::(tokenize rest)
  else if Re.execp lt input then (* Less *)
    let group = Re.exec lt input in
    let rest = Re.Group.get group 1 in
    Tok_Less::(tokenize rest)
  else if Re.execp l_or input then (* Or *)
    let group = Re.exec l_or input in
    let rest = Re.Group.get group 1 in
    Tok_Or::(tokenize rest)
  else if Re.execp l_and input then (* And *)
    let group = Re.exec l_and input in
    let rest = Re.Group.get group 1 in
    Tok_And::(tokenize rest)
  else if Re.execp l_not input then (* Not *)
    let group = Re.exec l_not input in
    let rest = Re.Group.get group 1 in
    Tok_Not::(tokenize rest)
  else if Re.execp l_if input then (* If *)
    let group = Re.exec l_if input in
    let rest = Re.Group.get group 1 in
    Tok_If::(tokenize rest)
  else if Re.execp l_then input then (* Then *)
    let group = Re.exec l_then input in
    let rest = Re.Group.get group 1 in
    Tok_Then::(tokenize rest)
  else if Re.execp l_else input then (* Else *)
    let group = Re.exec l_else input in
    let rest = Re.Group.get group 1 in
    Tok_Else::(tokenize rest)
  else if Re.execp add input then (* Add *)
    let group = Re.exec add input in
    let rest = Re.Group.get group 1 in
    Tok_Add::(tokenize rest)
  else if Re.execp sub input then (* Sub *)
    let group = Re.exec sub input in
    let rest = Re.Group.get group 1 in
    Tok_Sub::(tokenize rest)
  else if Re.execp mul input then (* Mult *)
    let group = Re.exec mul input in
    let rest = Re.Group.get group 1 in
    Tok_Mult::(tokenize rest)
  else if Re.execp div input then (* Div *)
    let group = Re.exec div input in
    let rest = Re.Group.get group 1 in
    Tok_Div::(tokenize rest)
  else if Re.execp conc input then (* Concat *)
    let group = Re.exec conc input in
    let rest = Re.Group.get group 1 in
    Tok_Concat::(tokenize rest)
  else if Re.execp l_let input then (* Let *)
    let group = Re.exec l_let input in
    let rest = Re.Group.get group 1 in
    Tok_Let::(tokenize rest)
  else if Re.execp l_rec input then (* Rec *)
    let group = Re.exec l_rec input in
    let rest = Re.Group.get group 1 in
    Tok_Rec::(tokenize rest)
  else if Re.execp l_in input then (* In *)
    let group = Re.exec l_in input in
    let rest = Re.Group.get group 1 in
    Tok_In::(tokenize rest)
  else if Re.execp l_def input then (* Def *)
    let group = Re.exec l_def input in
    let rest = Re.Group.get group 1 in
    Tok_Def::(tokenize rest)
  else if Re.execp l_fun input then (* Fun *)
    let group = Re.exec l_fun input in
    let rest = Re.Group.get group 1 in
    Tok_Fun::(tokenize rest)
  else if Re.execp dbl_sm input then (* Double semi *)
    let group = Re.exec dbl_sm input in
    let rest = Re.Group.get group 1 in
    Tok_DoubleSemi::(tokenize rest)
  else if Re.execp semi input then (* Semi *)
    let group = Re.exec semi input in
    let rest = Re.Group.get group 1 in
    Tok_Semi::(tokenize rest)
  else if Re.execp id input then (* IDs *)
    let group = Re.exec id input in
    let in_id = Re.Group.get group 1 in
    let rest = Re.Group.get group 2 in
    Tok_ID(in_id)::(tokenize rest)
  else
    raise (InvalidInputException input)