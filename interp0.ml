(* Tokenizer *)

type token_class = TTString | TTBinaryOp | TTUnaryOp | TTOpenParen | TTCloseParen | TTAssign | TTEmpty
type token = {str: string; cls: token_class}

let empty_token = {str=""; cls=TTEmpty}

let string_to_token str =
  match str with
    "=" -> TTAssign
  | "(" -> TTOpenParen
  | ")" -> TTCloseParen
  | "==" -> TTBinaryOp
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
    {cls=TTString} ->
      let leaf_value = Leaf {value=current_token; cls=CLP; position=position} in
      if (node_cls stack_top) = CLEL then
        (PopPush (Node {cls=CLE; children=[stack_top; leaf_value]}), Consume)
      else
        (Push leaf_value, Consume)
  | {cls=TTUnaryOp} ->
      if node_cls stack_top = CLP then
        let leaf_value = Leaf {value=current_token; cls=CLUP; position=position} in
        (PopPush (Node {cls=CLP; children=[stack_top; leaf_value]}), Consume)
      else
        raise Stack_error
  | {cls=TTBinaryOp} ->
      if List.mem (node_cls stack_top) [CLE; CLP] then
        let leaf_value = Leaf {value=current_token; cls=CLOP; position=position} in
        (PopPush (Node {cls=CLEL; children=[stack_top; leaf_value]}), Consume)
      else
        raise Stack_error
  | {cls=TTAssign} ->
      if List.mem (node_cls stack_top) [CLE; CLP] then
        (PopPush (Node {cls=CLAL; children=[stack_top]}), Consume)
      else
        raise Stack_error
  | _ -> raise Token_error

let rec _forward_pass tokens stack position =
  match tokens with
    [] -> stack
  | tokens_hd :: tokens_tl ->
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

(*
 let op_func str =
  match str with
    "+" -> (fun left_node right_node state -> (interpret left_node).[0] ^ (interpret right_node).[0])
  | "++" -> (fun left_node right_node state ->
      (next_val (interpret left_node).[0] state) ^ (next_val (interpret right_node).[0] state))
*)
  (* TODO *)

exception Interpret_CLP_snd_error
exception Interpret_no_match

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
  | Node {cls=CLEL; children=(child_fst :: Leaf {cls=CLOP; value=snd_value} :: [])} -> ("none", state) (* TODO *)
  | _ -> raise Interpret_no_match
