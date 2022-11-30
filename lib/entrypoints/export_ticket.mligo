#import "../data/token.mligo" "Token"
#import "../data/ledger.mligo" "Ledger"
#import "../data/storage.mligo" "Storage"

type storage = Storage.t

type ticket_to_export = [@layout:comb] {
      from_ : address;
      token_id : Token.t;
      amount : nat 
   }

type destination = [@layout:comb]
   | Single of (nat * bytes option) ticket contract
   | Multiple of (nat * bytes option) ticket list contract

type singler_export_ticket = [@layout:comb] {
      destination : destination;
      tickets_to_export : ticket_to_export list
   }

type export_ticket = singler_export_ticket list

type t = export_ticket

let export_ticket (type a) (_exported_ticket : export_ticket) (storage: a storage) : operation list * a storage =
   ([]: operation list), storage
