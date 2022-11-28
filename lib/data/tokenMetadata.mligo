type data = {
    token_id  : nat;
    token_info: (string,bytes)map
}

type t = (nat, data) big_map
