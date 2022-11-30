#import "../data/errors.mligo" "Errors"
#import "../data/token.mligo" "Token"
#import "../data/ledger.mligo" "Ledger"
#import "../data/storage.mligo" "Storage"

type storage = Storage.t

(*
    (list %import_ticket
        (pair
            (address %to_)
            (list %tickets_to_import
                (ticket (pair nat (option bytes)))
            )
        )
*)

type imported_ticket = (Token.t * bytes option) ticket

type tickets_to_import_to = { 
      to_ : address; 
      tickets_to_import: imported_ticket list
   }

type import_ticket = tickets_to_import_to list

type t = import_ticket

[@inline]
let assert_ticketer_is_self_address (ticketer: address) : unit =
    assert_with_error (ticketer = (Tezos.get_self_address ())) Errors.invalid_ticket

[@inline]
let import_ticket (to_:address) (imported_ticket: imported_ticket) (ops, ledger: operation list * Ledger.t) : operation list * Ledger.t =
    let (ticketer, ((token_id, _), amount)), _ = Tezos.read_ticket imported_ticket in
    let () = assert_ticketer_is_self_address ticketer in
    let ledger = Ledger.increase_token_amount_for_user ledger to_ token_id amount in
    ops, ledger

[@inline]
let import_tickets_to (tickets_to_import_to: tickets_to_import_to) (ops, ledger: operation list * Ledger.t) : operation list * Ledger.t =
    let { to_; tickets_to_import } = tickets_to_import_to in
    List.fold_left (fun (r,t) -> import_ticket to_ t r) (ops, ledger) tickets_to_import

[@inline]
let import_tickets (type a) (import_ticket : import_ticket) (storage: a storage) : operation list * a storage =
    let ops, ledger = List.fold_left (fun (r,t) -> import_tickets_to t r) ([], Storage.get_ledger storage) import_ticket in
    ops, Storage.set_ledger storage ledger
