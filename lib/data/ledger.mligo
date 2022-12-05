#import "errors.mligo" "Errors"
#import "token.mligo" "Token"

type owner = address
type amount_ = nat

type 'k t = ('k, nat) big_map
type 'k ledger_and_make = { 
    data: 'k t; 
    make_key: address -> Token.t -> 'k

}

let get_for_user 
      (type k) 
      (ledger_and_make: k ledger_and_make) 
      (owner: owner) 
      (token_id: Token.t) 
      : amount_ =
  match Big_map.find_opt (ledger_and_make.make_key owner token_id) ledger_and_make.data with 
    Some (a) -> a 
  | None -> 0n

let set_for_user 
      (type k) 
      (ledger_and_make: k ledger_and_make) 
      (owner: owner) 
      (token_id: Token.t) 
      (amount_:amount_) 
      : k ledger_and_make =
  let data = Big_map.update (ledger_and_make.make_key owner token_id) (Some amount_) ledger_and_make.data in
  { ledger_and_make with data }

let decrease_token_amount_for_user 
      (type k) 
      (ledger_and_make: k ledger_and_make) 
      (from_: owner) 
      (token_id: Token.t) 
      (amount_: nat)
      : k ledger_and_make =
  let balance_ = get_for_user ledger_and_make from_ token_id in
  let ()       = assert_with_error (balance_ >= amount_) Errors.ins_balance in
  let balance_ = abs (balance_ - amount_) in
  let ledger   = set_for_user ledger_and_make from_ token_id balance_ in
  ledger

let increase_token_amount_for_user 
      (type k) 
      (ledger_and_make: k ledger_and_make) 
      (to_: owner) 
      (token_id: Token.t)
      (amount_: nat)
      : k ledger_and_make =
  let balance_ = get_for_user ledger_and_make to_ token_id in
  let balance_ = balance_ + amount_ in
  let ledger   = set_for_user ledger_and_make to_ token_id balance_ in
  ledger

(* Possible types as defined in the TZIP-12 *)

type ledger = t

module Single_asset = struct 
  type t = address
  let make_key (a:address) (_: Token.t) = a
  let ledger_and_make (data: t ledger) : t ledger_and_make = { data; make_key }
end

module Multi_asset = struct
  type t = address * Token.t
  let make_key (a:address) (t: Token.t) = a,t
  let ledger_and_make (data: t ledger) : t ledger_and_make = { data; make_key }
end

