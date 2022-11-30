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

type ticket_to_import = (Token.t * bytes option) ticket

type tickets_to_import_to = { 
      to_ : address; 
      tickets_to_import: ticket_to_import list
   }

type import_ticket = tickets_to_import_to list

type t = import_ticket

let assert_ticketer_is_self_address (ticketer: address) : unit =
    assert_with_error (ticketer = (Tezos.get_self_address ())) Errors.invalid_ticket

let import_ticket (to_:address) (ticket_to_import: ticket_to_import) (ledger: Ledger.t) (ops: operation list) : operation list * Ledger.t =
    let (ticketer, ((token_id, _), amount)), _ = Tezos.read_ticket ticket_to_import in
    let () = assert_ticketer_is_self_address ticketer in
    let ledger = Ledger.increase_token_amount_for_user ledger to_ token_id amount in
    ops, ledger

let import_tickets_to (tickets_to_import_to: tickets_to_import_to) (ledger: Ledger.t) (ops: operation list) : operation list * Ledger.t =
    let { to_; tickets_to_import } = tickets_to_import_to in
    List.fold_left (fun ((o,l),t) -> import_ticket to_ t l o) (ops, ledger) tickets_to_import

let import_tickets (type a) (import_ticket : import_ticket) (storage: a storage) : operation list * a storage =
    let ops, ledger = List.fold_left (fun ((o,l),t) -> import_tickets_to t l o) ([], Storage.get_ledger storage) import_ticket in
    ops, Storage.set_ledger storage ledger
