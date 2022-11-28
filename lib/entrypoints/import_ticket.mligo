#import "../data/token.mligo" "Token"
#import "../data/ledger.mligo" "Ledger"
#import "../data/storage.mligo" "Storage"

type storage = Storage.t

type tickets_to_import_to = { 
      to_ : address; 
      tickets_to_import: (nat * bytes option) ticket
   }

type import_ticket = tickets_to_import_to list

let import_ticket (_imported_ticket : import_ticket) (storage: storage) : operation list * storage =
   ([]: operation list), storage
