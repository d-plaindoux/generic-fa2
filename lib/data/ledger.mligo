#import "errors.mligo" "Errors"
#import "token.mligo" "Token"

type owner    = address
type amount_  = nat
type t = ((owner * Token.t), amount_) big_map

let get_for_user (ledger:t) (owner: owner) (token_id: Token.t) : amount_ =
  match Big_map.find_opt (owner, token_id) ledger with 
    Some (a) -> a 
  | None -> 0n

let set_for_user (ledger:t) (owner: owner) (token_id: Token.t) (amount_:amount_) : t =
  Big_map.update (owner,token_id) (Some amount_) ledger

let decrease_token_amount_for_user (ledger: t) (from_: owner) (token_id: Token.t) (amount_: nat): t =
  let balance_ = get_for_user ledger from_ token_id in
  let ()       = assert_with_error (balance_ >= amount_) Errors.ins_balance in
  let balance_ = abs (balance_ - amount_) in
  let ledger   = set_for_user ledger from_ token_id balance_ in
  ledger

let increase_token_amount_for_user (ledger: t) (to_: owner) (token_id: Token.t) (amount_: nat): t =
  let balance_ = get_for_user ledger to_ token_id in
  let balance_ = balance_ + amount_ in
  let ledger   = set_for_user ledger to_ token_id balance_ in
  ledger
