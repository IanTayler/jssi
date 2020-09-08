(* Tokenizer *)

type token_class = TTString | TTBinaryOp | TTUnaryOp | TTOpenParen | TTCloseParen | TTAssign | TTEmpty
type token = {str: string; cls: token_class}

let empty_token = {str=""; cls=TTEmpty}

let string_to_token str =
  match str with
    "=" -> TTAssign
  | "(" -> TTOpenParen
  | ")" -> TTCloseParen
  | "==" -> TTAssign
  | "===" -> TTBinaryOp
  | "+" -> TTBinaryOp
  | "++" -> TTBinaryOp
  | "*" -> TTUnaryOp
  | "'" -> TTUnaryOp
  | _ -> TTString

exception Separate_error

let rec _separate_unary str acc =
  let str_length = String.length str in
  let last_char = if str_length > 0 then Some (Str.last_chars str 1) else None in
  match last_char with
    Some ch ->
      if List.mem ch ["*"; "'"] then
        _separate_unary (Str.string_before str (str_length - 1)) (ch :: acc)
      else
        str :: acc
  | None -> acc

let separate_unary str = _separate_unary str []

let tokenize str =
  let strings = String.split_on_char ' ' str in
  let strings = List.concat (List.map separate_unary strings) in
  List.map (fun str -> {str=str; cls=(string_to_token str)}) strings


(* Parser *)

type node_class = CLUP | CLP | CLE | CLA | CLAL | CLEL | CLOP | CLEmpty
type 'a ast =
  Leaf of {value: 'a; cls: node_class; position: int}
  | Node of {cls: node_class; children: 'a ast list}

let empty_ast = Leaf {value=empty_token; cls=CLEmpty; position=0}

let node_cls node =
  match node with
    Leaf {cls=cls} -> cls
  | Node {cls=cls} -> cls

type token_action = Consume | Stay
type 'a stack_action = PopPush of 'a ast | PopSkip | Push of 'a ast

exception Token_error
exception Stack_error

let forward_step stack_top position current_token =
  match current_token with
    {cls=TTBinaryOp} ->
      if List.mem (node_cls stack_top) [CLE; CLP] then
        let leaf_value = Leaf {value=current_token; cls=CLOP; position=position} in
        (PopPush (Node {cls=CLEL; children=[stack_top; leaf_value]}), Consume)
      else
        raise Stack_error
  | {cls=TTUnaryOp} ->
      if List.mem (node_cls stack_top) [CLE; CLP] then
        let leaf_value = Leaf {value=current_token; cls=CLUP; position=position} in
        (PopPush (Node {cls=CLP; children=[stack_top; leaf_value]}), Consume)
      else
        raise Stack_error
  | {cls=TTAssign} ->
      let leaf_value = Leaf {value=current_token; cls=CLOP; position=position} in
      if List.mem (node_cls stack_top) [CLE; CLP] then
        (PopPush (Node {cls=CLAL; children=[stack_top; leaf_value]}), Consume)
      else
        raise Stack_error
  | _ -> raise Token_error

exception P_expr_error_string
exception P_expr_error_none

let rec _form_p_expr tokens position acc =
  match tokens with
    [] -> (acc, tokens, position)
  | token_hd :: tokens_tl ->
      match acc with
        None ->
          _form_p_expr tokens_tl (position + 1) (Some (Leaf {cls=CLP; value=token_hd; position=position}))
      | Some node ->
          if token_hd.cls = TTUnaryOp then
            let leaf_value = Leaf {value=token_hd; cls=CLUP; position=position} in
            _form_p_expr tokens_tl (position + 1) (Some (Node {cls=CLP; children=[node; leaf_value]}))
          else if token_hd.cls = TTString then
            raise P_expr_error_string
          else
            (acc, tokens, position)

let form_p_expr tokens position =
  let opt_node, new_tokens, new_position = _form_p_expr tokens position None in
  match opt_node with
    None -> raise P_expr_error_none
  | Some node -> (node, new_tokens, new_position)

let rec _forward_pass tokens stack position =
  match tokens with
    [] -> stack
  | tokens_hd :: tokens_tl ->
      if tokens_hd.cls = TTString then
        let p_expr, new_tokens, new_position = form_p_expr tokens position in
        let stack_hd :: stack_tl = stack in
        if node_cls stack_hd = CLEL then
          _forward_pass new_tokens ((Node {cls=CLE; children=[stack_hd; p_expr]}) :: stack_tl) new_position
        else
          _forward_pass new_tokens (p_expr :: stack) new_position
      else
        let stack_action, token_action = forward_step (List.hd stack) position tokens_hd in
        let new_tokens = if token_action = Consume then tokens_tl else tokens in
        let new_position = if token_action = Consume then position + 1 else position in
        match stack_action with
          PopPush node -> _forward_pass new_tokens (node :: (List.tl stack)) new_position
        | Push node -> _forward_pass new_tokens (node :: stack) new_position
        | _ -> raise Stack_error

let forward_pass tokens = _forward_pass tokens [empty_ast] 1

exception Backward_error
exception Backward_unexpected_error

let rec backward_pass stack =
  match stack with
    hd :: (Leaf {cls=CLEmpty}) :: _ -> hd
  | hd :: snd :: tl ->
      if List.mem (node_cls hd) [CLE; CLP] && (node_cls snd) = CLAL then
        backward_pass ((Node {cls=CLA; children=[snd; hd]}) :: tl)
      else
        raise Backward_error
  | _ -> raise Backward_unexpected_error

let parse str = backward_pass (forward_pass (tokenize str))

(* Interpreter *)
module State = Map.Make(String)

let state_of_list l = State.of_seq (List.to_seq l)

let next_val var state =
  let state_val = State.find_opt var state in
  match state_val with
    None -> var
  | Some value -> value

let rec last_val var state =
  let state_val = State.find_opt var state in
  match state_val with
    None -> var
  | Some value -> last_val value state

let assign var value state = State.add var value state

exception Interpret_CLP_snd_error
exception Interpret_CLE_children_error
exception Interpret_CLA_children_error
exception Interpret_CLA_op_error
exception Interpret_no_match
exception Interpret_no_op

let op_func str =
  match str with
    "+" -> (fun left_value right_value state -> left_value ^ right_value)
  | "++" -> (fun left_value right_value state ->
      (next_val left_value state) ^ (next_val right_value state))
  | _ -> raise Interpret_no_op

let rec interpret ast state =
  match ast with
    Leaf leaf_node -> (leaf_node.value.str, state)
  | Node {cls=CLP; children=(child_fst :: Leaf {cls=CLUP; value=snd_value} :: [])} ->
      let fst_value_str, _ = interpret child_fst state in
      if snd_value.str = "*" then
        (next_val fst_value_str state, state)
      else if snd_value.str = "'" then
        (last_val fst_value_str state, state)
      else
        raise Interpret_CLP_snd_error
  | Node {cls=CLAL; children=(child_fst :: Leaf {cls=CLOP; value=snd_value} :: [])} ->
      interpret child_fst state
  | Node {cls=CLA; children=((Node child_fst) :: child_snd :: [])} ->
      let child_fst_value, new_state_fst = interpret (Node child_fst) state in
      let child_snd_value, new_state_snd = interpret child_snd new_state_fst in
      begin
        match child_fst.children with
          _ :: Leaf {cls=CLOP; value=op_value} :: [] ->
            if op_value.str = "=" then
              ("()", assign child_fst_value child_snd_value new_state_snd)
            else if op_value.str = "==" then
              ("()", assign
                (next_val child_fst_value new_state_snd)
                child_snd_value
                new_state_snd
              )
            else raise Interpret_CLA_op_error
        | _ -> raise Interpret_CLA_children_error
      end
  | Node {cls=CLEL; children=(child_fst :: Leaf {cls=CLOP; value=snd_value} :: [])} ->
      interpret child_fst state
  | Node {cls=CLE; children=((Node child_fst) :: child_snd :: [])} ->
      let child_fst_value, new_state_fst = interpret (Node child_fst) state in
      let child_snd_value, new_state_snd = interpret child_snd new_state_fst in
      begin
        match child_fst.children with
          _ :: Leaf {cls=CLOP; value=op_value} :: [] ->
            ((op_func op_value.str) child_fst_value child_snd_value new_state_snd, new_state_snd)
        | _ -> raise Interpret_CLE_children_error
      end
  | _ -> raise Interpret_no_match

(* Main loop *)

let rec loop state =
  print_string ">> ";
  let out, new_state = interpret (parse (read_line ())) state in
  if out <> "()" then
    begin
      print_string out;
      print_string "\n";
    end
  else
    print_string "\n";
  loop new_state

let () =
  try
    loop State.empty
  with
    End_of_file -> print_string "\n";;
