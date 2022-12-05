#import "../data/errors.mligo" "Errors"
#import "../data/metadata.mligo" "Metadata"
#import "../data/token.mligo" "Token"
#import "../data/ledger.mligo" "Ledger"
#import "../data/operators.mligo" "Operators"
#import "../data/approvals.mligo" "Approvals"
#import "../data/tokenMetadata.mligo" "TokenMetadata"
#import "../data/storage.mligo" "Storage"

#import "../entrypoints/transfer.mligo" "Transfer"
#import "../entrypoints/balance_of.mligo" "Balance_of"
#import "../entrypoints/update.mligo" "Update"
#import "../entrypoints/approve.mligo" "Approve"
#import "../entrypoints/export_ticket.mligo" "Export_ticket"
#import "../entrypoints/import_ticket.mligo" "Import_ticket"

type storage = Storage.t

type ledger = Ledger.t
type ledger_and_make = Ledger.ledger_and_make

type parameter = [@layout:comb]
   | Transfer of Transfer.transfer
   | Balance_of of Balance_of.balance_of
   | Update_operators of Update.update_operators

let main 
         (type a k) 
         (ledger_and_make:k ledger -> k ledger_and_make) 
         ((p,s):(parameter * (a,k) storage)) 
         : operation list * (a,k) storage = 
   match p with
   | Transfer         p -> Transfer.transfer p s ledger_and_make
   | Balance_of       p -> Balance_of.balance_of p s ledger_and_make
   | Update_operators p -> Update.update_ops p s

(*
   Views corner
*)

let balance_of 
         (type a k) 
         (ledger_and_make:k ledger -> k ledger_and_make) 
         (parameter, storage : (address * Token.t) * (a,k) storage) 
         : nat =
   let (owner, token_id) = parameter in
   Ledger.get_for_user (ledger_and_make storage.ledger) owner token_id

let total_supply 
         (type a k) 
         ((token_id, storage) : (nat * (a,k) storage)) 
         : nat =
   if Storage.token_exist storage token_id 
   then 1n 
   else 0n
