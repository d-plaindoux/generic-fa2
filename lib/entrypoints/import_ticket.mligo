#import "../data/errors.mligo" "Errors"
#import "../data/token.mligo" "Token"
#import "../data/ledger.mligo" "Ledger"
#import "../data/storage.mligo" "Storage"

type storage = Storage.t

type tickets_to_import_to = { 
      to_ : address; 
      tickets_to_import: (Token.t * bytes option) ticket
   }

type import_ticket = tickets_to_import_to list

let assert_ticketer_is_self_address (ticketer: address) : unit =
    assert_with_error (ticketer = (Tezos.get_self_address ())) Errors.invalid_ticket

let import_ticket (type a) (imported_ticket : tickets_to_import_to) (storage: a storage) : operation list * a storage =
    let { to_; tickets_to_import} = imported_ticket in
    let (ticketer, ((token_id, _), amount)), _ = Tezos.read_ticket tickets_to_import in
    let () = assert_ticketer_is_self_address ticketer in
    let ledger = Ledger.increase_token_amount_for_user storage.ledger to_ token_id amount in
    ([]: operation list), Storage.set_ledger storage ledger

let import_ticket (_imported_ticket : import_ticket) (storage: storage) : operation list * storage =
    ([]: operation list), storage
