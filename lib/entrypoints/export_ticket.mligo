#import "../data/token.mligo" "Token"
#import "../data/ledger.mligo" "Ledger"
#import "../data/storage.mligo" "Storage"

type storage = Storage.t

(*
   (list %export_ticket
      (pair
         (or %destination (contract (ticket (pair nat (option bytes))))
               (contract (list (ticket (pair nat (option bytes)))))
         )
         (list %tickets_to_export
               (pair
                  (address %from_)
                  (nat %token_id)
                  (nat %amount)
               )
         )
      )
   )
*)

type exported_ticket = (nat * bytes option) ticket 

type ticket_to_export = [@layout:comb] {
      from_ : address;
      token_id : Token.t;
      amount : nat 
   }

type destination = [@layout:comb]
   | Single of exported_ticket contract
   | Multiple of exported_ticket list contract

type export_tickets_to = [@layout:comb] {
      destination : destination;
      tickets_to_export : ticket_to_export list
   }

type export_ticket = export_tickets_to list

type t = export_ticket

let create_ticket (ticket_to_export: ticket_to_export) (ledger: Ledger.t) : (Ledger.t * exported_ticket) = 
   let { from_; token_id; amount } = ticket_to_export in
   let ledger = Ledger.decrease_token_amount_for_user ledger from_ token_id amount in
   let ticket = Tezos.create_ticket (token_id, None) amount in
   ledger, ticket

let create_tickets (tickets_to_export: ticket_to_export list) (ledger: Ledger.t) : (Ledger.t * exported_ticket list) = 
   List.fold_left (fun ((l,lt), t) -> let (l, t) = create_ticket t l in (l, t :: lt)) (ledger, []) tickets_to_export

let send_tickets_to (destination: destination) (tickets: exported_ticket list) (ops: operation list) : operation list =
   match destination with
   | Single contract -> List.fold_left (fun (ops, ticket) -> Tezos.transaction ticket 0tez contract :: ops) ops tickets
   | Multiple contract -> Tezos.transaction tickets 0tez contract :: ops

let export_ticket_to (export_tickets_to: export_tickets_to) (ops, ledger: operation list * Ledger.t): (operation list * Ledger.t) = 
   let { destination; tickets_to_export } = export_tickets_to in
   let ledger, tickets = create_tickets tickets_to_export ledger in
   let ops = send_tickets_to destination tickets ops in
   ops, ledger

let export_tickets (type a) (export_ticket: export_ticket) (storage: a storage) : operation list * a storage =
   let ops, ledger = List.fold_left (fun (r,t) -> export_ticket_to t r) ([], Storage.get_ledger storage) export_ticket in
   ops, Storage.set_ledger storage ledger
