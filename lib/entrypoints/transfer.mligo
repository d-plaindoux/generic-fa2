#import "../data/token.mligo" "Token"
#import "../data/approvals.mligo" "Approvals"
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

let authorize_transfer (type a) (from_: address) (token_id: Token.t) (amount: nat) (approvals:Approvals.t) (storage: a storage) : Approvals.t =
   match Storage.get_operators storage with
   | Some operators -> let () = Operators.assert_authorisation operators from_ token_id in approvals
   | None           -> Approvals.decrease_approved_amount approvals from_ (Tezos.get_sender ()) token_id amount

let atomic_trans (type a) (from_:address) (storage: a storage) ((ledger, approvals), transfer:(Ledger.t * Approvals.t) * atomic_trans) =
   let { to_; token_id; amount = amount_ } = transfer in
   let ()        = Storage.assert_token_exist storage token_id in
   let approvals = authorize_transfer from_ token_id amount_ approvals storage in
   let ledger    = Ledger.decrease_token_amount_for_user ledger from_ token_id amount_ in
   let ledger    = Ledger.increase_token_amount_for_user ledger to_   token_id amount_ in
   ledger, approvals

let transfer_from (type a) (storage: a storage) ((ledger, approvals), transfer : (Ledger.t * Approvals.t) * transfer_from ) =
   let { from_; tx } = transfer in 
   List.fold_left (atomic_trans from_ storage) (ledger, approvals) tx

let transfer_with_approvals (type a) (transfer: transfer) (storage: a storage) : operation list * a storage =
   let approvals = Storage.get_approvals storage in
   let ledger = Storage.get_ledger storage in
   let ledger,approvals = List.fold_left (transfer_from storage) (ledger, approvals) transfer in
   let storage = Storage.set_approvals storage approvals in
   let storage = Storage.set_ledger storage ledger in
   [], storage

let transfer (type a) (t: transfer) (s: a storage) : operation list * a storage =
   transfer_with_approvals t s