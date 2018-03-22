(*Auxiliary functions*)
type 'a dist = ('a * float) list

(*Combines elements with the same value.
  If in distribution [(a, p); (b, q);...] exsist value a and b that a = b
  then correct distribution to [(a, p *. q);....]. *)
let combine lst =
  let rec add_to dist (a, p) =
    match dist with
    | [] -> [(a,p)]
    | (b,q) :: lst ->
      if a = b
      then (b, p +. q) :: lst
      else (b, q) :: (add_to lst (a, p))
  in
  List.fold_left add_to [] lst

(* Takes the distribution and uses the function f on it.
   F can depend on the probability ...
   for example f (a) = [(b1, q1); (b2, q2); ...]. *)
let bind x f =
  combine (List.flatten (List.map (fun (a, p) -> List.map (fun (b, q) -> (b, p *. q)) (f a)) x))

(* X turns into a distribution that returns x almost certainly. *)
let dirac x = [(x, 1.0)]

(* Choose na element from the distribution based on its probability. *)
let choose lst =
  let rec choose' p = function
    | [] -> raise (Invalid_argument "choose")
    | (x, q) :: lst ->
      if p < q then x else choose' (p -. q) lst
  in
  choose' (Random.float 1.0) lst

(* Takes the list of elements and chooses each with the same probability. *)
let uniform lst =
  let p = 1.0 /. (float (List.length lst)) in
  choose (List.map (fun x -> (x, p)) lst)

let vector_product vector1 vector2 = (List.map2 (fun x y -> x *. y)) vector1 vector2

let rec expected_value dist =
  match dist with
  | [] -> raise (Invalid_argument "[]")
  | [(r, p)] -> List.map (fun x -> p *. x) r
  | (r, p)::tl -> (List.map2 (fun x y -> x +. y)) (List.map (fun x -> p *. x) r) (expected_value tl)

(*#########################################################################################################*)
    (* Input data. *)
let price = [1.; 1.; 1.] (* Prices of all securities available on our market. *)
(* All possible changes of interest rates in the next period.*)
let interestRates = [([1.01; 1.3; 1.1], 0.25); ([1.01; 1.1; 0.9], 0.25); ([1.01; 0.7; 0.8], 0.5)]

(*#########################################################################################################*)
(* Calculation of the expected value / distribution / one realization of the prices after [n] periods of time
   for all securities in our market.
   In this case, the lower part of the programs does not change.
   We only change the parameters (>> =), return and economicDevelopment. *)

let market_model_distribution interestRates price n =
  let (>>=) = bind in
  let return = dirac in
  let economicDevelopment = interestRates in

  let rec market price number_of_periods =
    if number_of_periods = 0 then
      return price
    else
      economicDevelopment >>= fun state ->
      market (vector_product price state) (number_of_periods - 1) >>= (fun k -> return k)
  in
  market price n

let market_model_expected_value interestRates price n =
  let (>>=) x f = f x in
  let return x = x in
  let economicDevelopment = expected_value interestRates in

  let rec market price number_of_periods =
    if number_of_periods = 0 then
      return price
    else
      economicDevelopment >>= fun state ->
      market (vector_product price state) (number_of_periods - 1) >>= (fun k -> return k)
  in
  market price n


  let market_model_realization interestRates price n =
    let (>>=) x f = f x in
    let return x = x in
    let economicDevelopment = choose interestRates in

    let rec market price number_of_periods =
      if number_of_periods = 0 then
        return price
      else
        economicDevelopment >>= fun state ->
        market (vector_product price state) (number_of_periods - 1) >>= (fun k -> return k)
    in
    market price n
