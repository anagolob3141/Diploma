type 'a finite_dist = ('a * float) list

module type MONADA =
sig
  type 'a t
  val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
  val return : 'a -> 'a t
end

module type TRG =
sig
  include MONADA
  val obresti : 'a finite_dist -> 'a t
end


module Obrestuj_porazdelitev =
struct
  type 'a t = 'a finite_dist

  let return x = [(x, 1.)]

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

  let obresti dist = dist

end


module Obrestuj_realizacija =
struct
  type 'a t = 'a

  let return x = x

  let choose dist =
    let rec choose' p = function
      | [] -> raise (Invalid_argument "choose")
      | (x, q) :: tl ->
        if p < q then x else choose' (p -. q) tl
    in
    choose' (Random.float 1.0) dist

  let (>>=) x f = f x

  let obresti dist = choose dist
end

module Simulacija_trga (T : TRG) =
struct
  open T

  let simulacija_trga s0 n dist =
    let rec korak k s =
      if k >=  n then
        return s
      else
        (obresti dist) >>= fun stanje ->
        let s_novi = (List.map2 ( *. ) stanje s) in
        korak (k + 1) s_novi
    in
    korak 0 s0;;

end

module Primer1 = Simulacija_trga (Obrestuj_porazdelitev);;
module Primer2 = Simulacija_trga (Obrestuj_realizacija);;


Primer1.simulacija_trga [20.] 3 [([1.02],0.7); ([0.95], 0.3)];;
Primer2.simulacija_trga [20.] 3 [([1.02],0.7); ([0.95], 0.3)];;
