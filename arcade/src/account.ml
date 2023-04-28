type t = {
  name : string;
  amount : int;
}

exception InsufficientFunds of int

(** [account name] is the initialization of an account with name [name] *)
let account s = { name = s; amount = 10 }

(** [add amt acc] is the account returned after [amt] has been added to the
    account*)
let add amt acc = { name = acc.name; amount = acc.amount + amt }

(** [sufficient amt acc] a true if [amt] is greater than or equal to the amount
    in [acc] *)
let sufficient amt (acc : t) = acc.amount >= amt

(** [deduct amt acc] is the account returned after [amt] has been deducted to
    the account. Requires [amt] is lesser than the amount in the account. Raises
    [Insufficient Funds] if it is not greater than or equal to.*)
let deduct amt acc =
  if sufficient amt acc then { name = acc.name; amount = acc.amount - amt }
  else raise (InsufficientFunds amt)

(** [balance acc] returns returns the amount of money in [acc]*)
let balance acc = acc.amount

(** [prob_data] is a list of 100 ints, fifty of them which will be 0, 30 of them
    will be between 1 and 74 and 20 of them will be between 25 and 100. This is
    meant to signify the wheel spinner that spins to pick a random gift. The
    players are able to pick an amount to deposit in their bank account *)

let rec prob_d n p lst =
  match n with
  | 0 -> lst
  | x -> p :: prob_d (x - 1) p lst

let shuffle d =
  let nd = List.map (fun c -> (Random.bits (), c)) d in
  let sond = List.sort compare nd in
  List.map snd sond

let prob_lst =
  prob_d 50 0 [] |> prob_d 20 10 |> prob_d 15 20 |> prob_d 10 50 |> prob_d 5 100
  |> shuffle

let get_init_amount pick = List.nth prob_lst (pick - 1)
