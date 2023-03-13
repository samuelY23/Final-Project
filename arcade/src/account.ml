type t = {
  name : string;
  amount : int;
}

exception InsufficientFunds of int

(** [account name] is the initialization of an account with name [name] *)
let account s = raise (Failure "klmd")

(** [add amt acc] is the account returned after [amt] has been added to the
    account*)
let add amt acc = raise (Failure "klmd")

(** [deduct amt acc] is the account returned after [amt] has been deducted to
    the account. Requires [amt] is lesser than the amount in the account. Raises
    [Insufficient Funds] if it is not greater than or equal to.*)
let deduct amt acc = raise (Failure "klmd")

(** [sufficient amt acc] a true if [amt] is greater than or equal to the amount
    in [acc] *)
let sufficient amt (acc : t) = raise (Failure "klmd")

(** [balance acc] returns returns the amount of money in [acc]*)
let balance acc = raise (Failure "klmd")

(** [prob_data] is a list of 100 ints, fifty of them which will be 0, 30 of them
    will be between 1 and 74 and 20 of them will be between 25 and 100. This is
    meant to signify the wheel spinner that spins to pick a random gift. The
    players are able to pick an amount to deposit in their bank account *)
let prob_data = raise (Failure "klmd")
