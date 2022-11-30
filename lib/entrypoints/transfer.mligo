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

let transfer_with_approvals (type a) (transfer: transfer) (storage: a storage) : operation list * a storage =
   let process_atomic_transfer (from_:address) ((ledger, approvals), t:(Ledger.t * Approvals.t) * atomic_trans) =
      let {to_;token_id;amount=amount_} = t in
      let ()        = Storage.assert_token_exist storage token_id in
      let approvals = authorize_transfer from_ token_id amount_ approvals storage in
      let ledger    = Ledger.decrease_token_amount_for_user ledger from_ token_id amount_ in
      let ledger    = Ledger.increase_token_amount_for_user ledger to_   token_id amount_ in
      ledger, approvals
   in
   let process_single_transfer ((ledger, approvals), transfer : (Ledger.t * Approvals.t) * transfer_from ) =
      let {from_;tx} = transfer in List.fold_left (process_atomic_transfer from_) (ledger, approvals) tx
   in
   let ledger,approvals = List.fold_left process_single_transfer (Storage.get_ledger storage, Storage.get_approvals storage) transfer in
   let storage = Storage.set_approvals storage approvals in
   let storage = Storage.set_ledger storage ledger in
   ([]: operation list), storage

let transfer (type a) (t: transfer) (s: a storage) : operation list * a storage =
   transfer_with_approvals t s