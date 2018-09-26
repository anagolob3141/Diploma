Random.self_init () (* Zagotovi generator naključnih št. z nakljičnim semenom.*)
type 'a finite_dist = ('a * float) list

module type MONAD =
sig
  type 'a t
  val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
  val return : 'a -> 'a t
end

(* MODULI, KI PREDPIŠEJO OBRESTNE MERE NA TRGU *)
module type OBRESTI =
sig
  include MONAD
  val obresti : 'a finite_dist  -> 'a t
end

module Porazdelitev_obresti =
struct
  type 'a t = 'a finite_dist

  let return x = [(x, 1.)]

  let obresti dist = dist

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

module Realizacija_obresti =
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

  let obresti dist = choose dist

  let (>>=) x f = f x

end


  (*MODULI, KI DOLOČIJO TRGOVALNO STRATEGIJO OZ. PORTFELIJ, KI GA BOMO KUPILI: *)
module type TRGUJ =
sig
  val trguj : float * float list -> float list
end

module Kupi_vse_trguj =
struct
  let trguj (premozenje, vrednostni_papirji) = vrednostni_papirji
end

module Kupi_prvega_trguj =
struct
  let trguj (premozenje, vrednostni_papirji) =
    match vrednostni_papirji with
    | [] -> raise (Invalid_argument "Kupi_prvega_trguj")
    | (x::xs) -> x :: List.map (fun _ -> 0.0) xs
end



(* FUNKTOR, KI DOLOČA OSNOVNI MODEL DELOVANJA TRGA: *)
module Simulacija_trga (O : OBRESTI) (T: TRGUJ) =
struct
  open O
  open T


(* Pomožne funkcije. *)

  let rec sestej = function
    | [] -> 0.0
    | d :: ds -> d +. sestej ds;;

  let obrestuj obresti delnice =
    List.map2 ( *. ) obresti delnice;;

  let omeji portfelj_max portfelj_novi =
    List.map2 (fun m x -> max 0.0 (min m x)) portfelj_max portfelj_novi;;

  let rec osvezi vrednostni_papirji portfelj =
    List.map2 ( -. ) vrednostni_papirji portfelj;;

  let round n x = (snd (modf (x *. 10. ** float_of_int n))) /. 10. ** float_of_int n;;

  (* Jedro funktorja*)
  (* Argumenti:
     - [n] število korakov simulacije
     - [banka_p] obrestna mera v banki za pozitivno stanje
     - [banka_n] obresnta mera v banki za negativno stanje
     - [banka0]  začetno stanje na banki
     - [vrednostni_papirji0]  začetna vrednost trga
     - [portfelj0] začetni portfelj
  *)

  let simulacija n banka_p banka_n banka0 vrednostni_papirji0 portfelj0 porazdelitev_obresti=

    (* En korak simulacije. *)
    let rec korak k banka vrednostni_papirji portfelj =
      if k >= n then
        return (banka, portfelj)
      else
        (* Trgujemo: *)
        let premozenje = banka +. sestej portfelj in
        let portfelj_max = List.map2 ( +. ) vrednostni_papirji portfelj in (* Celotna količina posameznih vrednostnih papirjev na trgu.*)
        let portfelj' = omeji portfelj_max (trguj (premozenje, portfelj_max)) in (* Izačuna novi portfelj in ga omeji z količino ponudbe na trgu. *)
        let banka' = premozenje -. sestej portfelj' in (* Izračuna novo stanje na banki.*)
        let vrednostni_papirji' = osvezi portfelj_max portfelj' in (* Izračun preostalo količino vrednostnih papirjev na trgu.*)
        (* Evolucija trga: *)
        (obresti porazdelitev_obresti) >>= fun stanje ->
        let portfelj'' = List.map (round 4) (obrestuj stanje portfelj') in (* Obrestuje portfelij.*)
        let vrednostni_papirji'' = List.map (round 4) (obrestuj stanje vrednostni_papirji') in (* Obrestuje trg.*)
        let banka'' = (round 4) (banka' *. (if banka' < 0.0 then banka_n else banka_p)) in (* Obrestuje stanje na bančnem računu.*)
        (korak (k + 1) banka'' vrednostni_papirji'' portfelj'')
      in
      korak 0 banka0 vrednostni_papirji0 portfelj0
  ;;
  let simulacija' n = simulacija n 1.02 0.9 200.0 [30.0; 40.0; 50.0] [0.0; 0.0; 0.0] [([1.2; 0.94; 0.99], 0.4); ([0.89; 1.1; 1.04], 0.6)];;

end


module Primer1 = Simulacija_trga (Porazdelitev_obresti) (Kupi_vse_trguj) (*Ne deluje za n = 2!*)
module Primer2 = Simulacija_trga (Realizacija_obresti) (Kupi_vse_trguj)
module Primer3 = Simulacija_trga (Realizacija_obresti) (Kupi_prvega_trguj)
module Primer4 = Simulacija_trga (Porazdelitev_obresti) (Kupi_prvega_trguj)

    (*
Primer1.simulacija' 1;;
Primer2.simulacija' 1;;
Primer2.simulacija' 1;;
Primer4.simulacija' 1;;
*)
