#import "../data/errors.mligo" "Errors"
#import "../data/token.mligo" "Token"
#import "../data/ledger.mligo" "Ledger"
#import "../data/storage.mligo" "Storage"

type storage = Storage.t

type imported_ticket = (Token.t * bytes option) ticket

type tickets_to_import_to = { 
      to_ : address; 
      tickets_to_import: imported_ticket list
   }

type import_ticket = tickets_to_import_to list

type t = import_ticket

type ledger = Ledger.t
type ledger_and_make = Ledger.ledger_and_make


[@inline]
let assert_ticketer_is_self_address (ticketer: address) : unit =
    assert_with_error (ticketer = (Tezos.get_self_address ())) Errors.invalid_ticket

let import_ticket_to 
            (type k) 
            (to_:address) 
            (imported_ticket: imported_ticket)
            (ops, ledger: operation list * k ledger_and_make)
            : operation list * k ledger_and_make =
    let (ticketer, ((token_id, _), amount)), _ = Tezos.read_ticket imported_ticket in
    let () = assert_ticketer_is_self_address ticketer in
    let ledger = Ledger.increase_token_amount_for_user ledger to_ token_id amount in
    ops, ledger

let import_tickets_to
            (type k)
            (tickets_to_import_to: tickets_to_import_to) 
            (ops, ledger: operation list * k ledger_and_make) 
            : operation list * k ledger_and_make =
    let { to_; tickets_to_import } = tickets_to_import_to in
    List.fold_left (fun (r,t) -> import_ticket_to to_ t r) (ops, ledger) tickets_to_import

let import_tickets 
            (type a k) 
            (import_ticket : import_ticket) 
            (storage: (a,k) storage) 
            (make : k ledger -> k ledger_and_make) 
            : operation list * (a,k) storage =
    let ops, ledger = List.fold_left (fun (r,t) -> import_tickets_to t r) ([], make (Storage.get_ledger storage)) import_ticket in
    ops, Storage.set_ledger storage ledger.data
