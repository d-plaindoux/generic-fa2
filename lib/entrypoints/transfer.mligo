#import "../data/token.mligo" "Token"
#import "../data/operators.mligo" "Operators"
#import "../data/ledger.mligo" "Ledger"
#import "../data/storage.mligo" "Storage"

type storage = Storage.t

type atomic_trans = [@layout:comb] {
      to_      : address;
      amount   : nat;
      token_id : Token.t;
   }

type transfer_from = {
      from_ : address;
      tx    : atomic_trans list
   }

type transfer = transfer_from list

type t = transfer

let transfer (type a) (t: transfer) (s: a storage) : operation list * a storage =
   let process_atomic_transfer (from_:address) (ledger, t:Ledger.t * atomic_trans) =
      let {to_;token_id;amount=amount_} = t in
      let ()     = Storage.assert_token_exist s token_id in
      let ()     = Operators.assert_authorisation s.operators from_ token_id in
      let ledger = Ledger.decrease_token_amount_for_user ledger from_ token_id amount_ in
      let ledger = Ledger.increase_token_amount_for_user ledger to_   token_id amount_ in
      ledger
   in
   let process_single_transfer (ledger, transfer :Ledger.t * transfer_from ) =
      let {from_;tx} = transfer in
      let ledger     = List.fold_left (process_atomic_transfer from_) ledger tx in
      ledger
   in
   let ledger = List.fold_left process_single_transfer s.ledger t in
   let storage = Storage.set_ledger s ledger in
   ([]: operation list), storage

