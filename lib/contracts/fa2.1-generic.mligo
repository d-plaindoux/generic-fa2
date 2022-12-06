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
type ledger_module = Ledger.ledger_module

type parameter = [@layout:comb]
   | Transfer of Transfer.transfer
   | Balance_of of Balance_of.balance_of
   | Update_operators of Update.update_operators
   | Approve of Approve.approvements
   | Export_ticket of Export_ticket.export_ticket
   | Import_ticket of Import_ticket.import_ticket

let main 
         (type a k v) 
         (make:(k,v) ledger -> (k,v) ledger_module) 
         ((p,s):(parameter * (a,k,v) storage)) 
         : operation list * (a,k,v) storage = 
   match p with
   | Transfer         p -> Transfer.transfer p s (make s.ledger)
   | Balance_of       p -> Balance_of.balance_of p s (make s.ledger)
   | Update_operators p -> Update.update_ops p s
   | Approve          p -> Approve.approve p s
   | Export_ticket    p -> Export_ticket.export_tickets p s (make s.ledger)
   | Import_ticket    p -> Import_ticket.import_tickets p s (make s.ledger)

(*
   Views corner
*)

let balance_of 
         (type a k v) 
         (make:(k,v) ledger -> (k,v) ledger_module) 
         ((owner, token_id), storage : (address * Token.t) * (a,k,v) storage) 
         : nat =
   let ledger_module = make storage.ledger in
   let value = Ledger.get_for_user ledger_module owner token_id in
   ledger_module.balance_of value

let total_supply 
         (type a k v) 
         ((token_id, storage) : (nat * (a,k,v) storage)) 
         : nat =
   if Storage.token_exist storage token_id 
   then 1n 
   else 0n
