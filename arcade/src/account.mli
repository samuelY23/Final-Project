type t
(** The abstract type representing the account *)

exception InsufficientFunds of int

val account : string -> t
(** [account name] is the initialization of an account with name [name] *)

val add : int -> t -> t
(** [add amt acc] is the account returned after [amt] has been added to the
    account*)

val deduct : int -> t -> t
(** [deduct amt acc] is the account returned after [amt] has been deducted to
    the account. Requires [amt] is lesser than the amount in the account. Raises
    [Insufficient Funds] if it is not greater than or equal to.*)

val sufficient : int -> t -> bool
(** [sufficient amt acc] a true if [amt] is greater than or equal to the amount
    in [acc] *)

val balance : t -> int
(** [balance acc] returns the amount of money in [acc]*)

val prob_lst : int list
(** [prob_data] is a list of 100 ints, fifty of them which will be 0, 30 of them
    will be between 1 and 74 and 20 of them will be between 25 and 100. This is
    meant to signify the wheel spinner that spins to pick a random gift. The
    players are able to pick an amount to deposit in their bank account *)

val get_init_amount : int -> int
(** [get_init_amount pick] returns a random initial amount for a player to start with. *)
