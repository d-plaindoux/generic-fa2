#import "../data/errors.mligo" "Errors"
#import "../data/token.mligo" "Token"
#import "../data/ledger.mligo" "Ledger"
#import "../data/storage.mligo" "Storage"

type storage = Storage.t

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
type ledger = Ledger.t
type ledger_and_make = Ledger.ledger_and_make

let create_ticket 
            (type k) 
            (ticket_to_export: ticket_to_export)
            (ledger: k ledger_and_make)
            : (k ledger_and_make * exported_ticket) = 
   let { from_; token_id; amount } = ticket_to_export in
   let ledger = Ledger.decrease_token_amount_for_user ledger from_ token_id amount in
   let ticket = Tezos.create_ticket (token_id, None) amount in
   let ticket = Option.unopt_with_error ticket Errors.cannot_create_ticket in
   ledger, ticket

let create_tickets 
            (type k) 
            (tickets_to_export: ticket_to_export list) 
            (ledger:k ledger_and_make)
            : (k ledger_and_make * exported_ticket list) = 
   List.fold_left (fun ((l,lt), t) -> let (l, t) = create_ticket t l in (l, t :: lt)) (ledger, []) tickets_to_export

let send_tickets_to 
            (destination: destination) 
            (tickets: exported_ticket list) 
            (ops: operation list) 
            : operation list =
   match destination with
   | Single contract -> 
      List.fold_left (fun (ops, ticket) -> Tezos.transaction ticket 0tez contract :: ops) ops tickets
   | Multiple contract -> 
      Tezos.transaction tickets 0tez contract :: ops

let export_ticket_to 
            (type k) 
            (export_tickets_to: export_tickets_to) 
            (ops, ledger: operation list * k ledger_and_make)
            : (operation list * k ledger_and_make) = 
   let { destination; tickets_to_export } = export_tickets_to in
   let ledger, tickets = create_tickets tickets_to_export ledger in
   let ops = send_tickets_to destination tickets ops in
   ops, ledger

let export_tickets
            (type a k) 
            (export_ticket: export_ticket) 
            (storage: (a, k) storage) 
            (make : k ledger -> k ledger_and_make) 
            : operation list * (a, k) storage =
   let ops, l = List.fold_left (fun (r,t) -> export_ticket_to t r) ([], make (Storage.get_ledger storage)) export_ticket in
   ops, Storage.set_ledger storage l.data
