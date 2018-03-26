type 'a finite_dist = ('a * float) list

module type MONAD =
sig
  type 'a t
  val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
  val return : 'a -> 'a t
end

module type MARKET_MODEL =
sig
  include MONAD
  val economicDevelopment : 'a finite_dist -> 'a t
end


module Price_distribution =
struct
  type 'a t = 'a finite_dist
  let return x = [(x, 1.)]
  let economicDevelopment dist = dist

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

  let (>>=) x f = combine (List.flatten (List.map (fun (a, p) -> List.map (fun (b, q) -> (b, p *. q)) (f a)) x))
end


module Price_expected_value =
struct
  type 'a t = 'a
  let return x = x

  let rec expected_value dist =
    match dist with
    | [] -> raise (Invalid_argument "[]")
    | [(r, p)] -> List.map (fun x -> p *. x) r
    | (r, p)::tl -> (List.map2 (fun x y -> x +. y)) (List.map (fun x -> p *. x) r) (expected_value tl)

  let economicDevelopment dist = expected_value dist

  let (>>=) x f = f x
end

module Price_realization =
struct
  type 'a t = 'a
  let return x = x

  let choose lst =
    let rec choose' p = function
      | [] -> raise (Invalid_argument "choose")
      | (x, q) :: lst ->
        if p < q then x else choose' (p -. q) lst
    in
    choose' (Random.float 1.0) lst

  let economicDevelopment dist = choose dist

  let (>>=) x f = f x
end


module Market_model (M : MARKET_MODEL) =
struct
  open M

  let vector_product vector1 vector2 = (List.map2 (fun x y -> x *. y)) vector1 vector2

  let market interestRates price number_of_periods =
    let rec market' price number_of_periods =
      if number_of_periods = 0 then
        return price
      else
        economicDevelopment interestRates >>= fun state ->
        market' (vector_product price state) (number_of_periods - 1) >>= (fun k -> return k)
    in
    market' price number_of_periods

  end


module Market_model_distribution = Market_model(Price_distribution)

module Market_model_realization = Market_model(Price_realization)

(* module Example3 = Market_model(Price_expected_value) doesn't work. *)
