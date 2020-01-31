(*******************************
       Projet AP3

   Prénom :
   Nom :

********************************)


(*********************************
  Mise en place de la bibliothèque
  graphique
********************************)

(*
   on charge la bilbiothèque dans l'interpréteur
*)
#load "graphics.cma"
;;

(*
  On ouvre le module
*)
open Graphics
;;

(*********************************
        Les boucles
********************************)

let rec forloop(r, n, next : 'a * int* ('a -> 'a)) : 'a =
  if n = 0 then r
  else forloop (next(r), n-1, next)
;;

let rec whileloop(r, cont, next : 'a * ('a -> bool) * ('a -> 'a)): 'a =
  if not(cont(r)) then r
  else whileloop(next(r), cont, next)
;;

(*********************************
        Exemples d'utilisation
	des boucles
 ********************************
(forloop((1, 1),
         5,
         (function (i, k) -> print_int i;
                             print_newline ();
                             (i+1, i*k)
         )
   )
)
;;

let affiche_compteurs(n, m : int * int) : unit =
  snd (forloop((0, ()),
               n,
               (function (i,()) -> snd (forloop((0, ()),
                                                m,
                                                (function (j, ()) -> print_int i;
					                             print_string " ";
					                             print_int j;
					                             print_newline (); (j+1, ()))));
                                   (i+1, ())))
    )
;;

affiche_compteurs(10,5)
;;

affiche_compteurs(5,10)
;;

whileloop((0,[]), (function (n, l) -> n < 10),
  (function (n, l) -> (n+1, n::l)))
;;


let square_root a =
  whileloop(1.0,
            (function x -> abs_float( x *. x -. a) >= 10. ** (-.10.)),
              (function x -> (0.5 *. (x +. a /. x))))
;;

square_root 2.
;;

********************************)


(*********************************
        Les types pour
	les images
*********************************)

type couleur = Noir | Blanc
;;

type picture = couleur array array
;;

type arbre = Feuille of couleur
	     | Noeud of arbre * arbre * arbre * arbre
;;


(*********************************
        Les fonctions
	draw_picture et read_pbm
*********************************)

(*
  Dessine une image donnée comme un tableau de couleurs
*)
let draw_picture img =
  let size = (Array.length img) in
    resize_window size size ;
    snd (
        forloop((0, ()),
                size,
	        (function (i, ())
	                  -> ( snd (forloop((0, ()),
                                            size,
			                    (function (j, () )
			                              -> ( set_color ( match img.(i).(j) with
			                                               | Blanc -> white
			                                               | Noir -> black ) ;
				                           plot i j ;
				                           (j+1, ()) )
			                    )
                                 )) ;
		               (i+1, ()) )
	        )
          )
      )
;;

(*
  Lecture d'un fichier pbm au format ascii
*)
let read_pbm filename =
  let file = open_in filename in
  let magic = input_line file in
    if magic <> "P1"
    then
      failwith "ce n'est pas un fichier pbm ascii"
    else
      let size_line = whileloop( (input_line file),
                                 (function l -> (l.[0] = '#' || l.[0] = ' ')),
	                         (function l -> input_line file))
      in
      let space_index = (String.index size_line ' ') in
      let size = int_of_string (String.sub size_line 0 space_index)
      and other_size = int_of_string (String.sub size_line (space_index + 1)
					 ((String.length size_line) - space_index - 1))
      in
	if (size <> other_size)
	then
	  failwith "ce n'est pas une image carrée"
	else
	  let img = Array.make_matrix size size Blanc in
	    snd (
	      try (
		whileloop((0, ()),
                          (function (k, ()) -> true),
		            (function (k, ()) -> ( match (input_char file) with
	                                           | '0' -> (k+1, ())
	                                           | '1' -> (img.(k mod size).(size - (k / size) - 1) <- Noir;
						             (k+1, ()))
	                                           | _   -> (k, ())
                                                 )
		            )
                  )
	      )
	      with End_of_file -> (0, ())
	    ) ;
	    close_in file ;
	    img
;;

(*********************************
     Exemples d'utilisation
     des fonctions read_pbm
     et draw_pbm
*********************************)

let d = read_pbm "portrait.pbm"
;;

open_graph ""
;;

draw_picture d
;;

draw_picture (read_pbm "avion.pbm")
;;

close_graph()
;;


(*********************************
       Mettez ici vos réponses
       aux questions et
       vos fonctions
*********************************)

(**************)
(* Question 1 *)
(**************)

let is_puiss_2 n =
  whileloop(n, (function(n) -> n mod 2 = 0) ,(function(n) -> n/2)) = 1;
;;


(**************)
(* Question 2 *)
(**************)

let img_test = [|
  [| Blanc; Noir; Blanc; Blanc |];
  [| Noir; Blanc; Blanc; Blanc |];
  [| Noir; Noir; Blanc; Noir |];
  [| Noir; Noir; Noir; Blanc |]
|]
;;

let img_test1 = [|
  [| Noir; Noir; Blanc; Blanc |];
  [| Noir; Noir; Blanc; Blanc |];
  [| Noir; Noir; Noir; Noir |];
  [| Noir; Noir; Noir; Noir |]
|]
;;

let arb_img_test = Noeud(Noeud(Feuille(Noir),Feuille(Blanc),Feuille(Noir),Feuille(Noir)),Feuille(Blanc),Feuille(Noir),Noeud(Feuille(Blanc),Feuille(Noir),Feuille(Noir),Feuille(Blanc)));;

let arb_img_test1 = Noeud(Feuille(Noir),Feuille(Blanc),Feuille(Noir),Feuille(Noir));;

*)

(**************)
(* Question 3 *)
(**************)

let random_img(t,n : int * int) :  picture =
  let img = Array.make_matrix t t Blanc in
  snd(forloop((t,img), n, (function(t,img) -> let i = Random.int(t) in
                                          let j = Random.int(t) in
                                          img.(i).(j) <- Noir;
                                          (t,img))))
;;

(* 
Exemples d'utilisation 
de la fonction random_img
 *)

random_img(4,4);;

(****************)
(* Question 4.1*)
(****************)

(*
          N
          |

    O-----O-----+
    | nw  | ne  |
    |     |     |
W-  O-----O-----+  -E
    | sw  | se  |
    |     |     |
    +-----+-----+

          |
          S

Découpage en région de l'image de taille kn
*)

let rec image_vers_arbre(k, img : int * picture) : arbre =
  let rec merge(img,i,j,k : picture * int * int * int) : arbre =
    if k <= 1 then Feuille img.(i).(j)
    else
      let kn  = k/2 in
      let nw = merge(img, i, j, kn)
      and ne = merge(img, i, (j + kn), kn)
      and sw = merge(img, (i + kn), j, kn)
      and se = merge(img, (i + kn), (j + kn), kn) in
      match nw, ne, sw, se with
      |Feuille a, Feuille b, Feuille c, Feuille d -> if a = b && b = c && c = d then nw else Noeud(nw, ne, sw, se)
      |_,_,_,_ -> Noeud(nw, ne, sw, se)
   in
   merge(img, 0, 0, k)
;;

(*
Exemples d'utilisation
de la fonction image_vers_arbre
*)

image_vers_arbre(4, img_test1);;

(****************)
(* Question 4.2 *)
(****************)

let remplir_carre (img, i, j, k, c : picture * int * int * int * couleur) : picture  =
  snd (forloop((i, img),
               k,
               (function (a,img) -> snd (forloop((j+1, ()),
                                                k,
                                                (function (b, ()) ->
                                                                     img.(a).(b) <- c;
					                            (b-1, ()))));
                                   (a+1, img)))
    )
;;

(* 
Exemples d'utilisation q
de la fonction remplir_carre
*)
remplir_carre(random_img(4,0),1,0,2, Noir);;


let  arbre_vers_image(k,arb : int * arbre) : picture =
  let img = Array.make_matrix k k Blanc in
  let rec fill_image(i, j, k, arb : int * int * int * arbre) : picture =
    match arb with
    |Feuille c  -> remplir_carre(img, i, j, k, c)
    |Noeud (nw, ne, sw, se) ->
      let kn = k/2 in
      fill_image(i, (j+kn), kn, nw);
      fill_image(i,j, kn, sw);
      fill_image((i+kn), (j+kn), kn, ne);
      fill_image((i+kn), j, kn, se);
  in
  fill_image(0, 0, k, arb)
;;

(*
Exemples d'utilisation 
de la fonction arbre_vers_image
*)

arbre_vers_image(4, arb_img_test1);;


(**************)
(* Question 5 *)
(**************)

let rec draw_tree k a =
  let rec do_draw i j k a = 
    match a with
    |Feuille Noir ->  Graphics.fill_rect i j k k
    |Feuille Blanc -> ()
    |Noeud (nw,ne,sw,se) ->
      let kn = k/2 in
      do_draw i (j+kn) kn nw ;
      do_draw (i+kn) (j+kn) kn ne ;
      do_draw i j kn sw ;
      do_draw (i+kn) j kn se
 in
 do_draw 0 0 k a
;;



(* 
Exemples d'utilisation 
de la fonction draw_tree
 *)

open_graph "" ;;
resize_window 256 256;;
draw_tree 256 arb_img_test1 ;;
close_graph()
;;

;;
(****************)
(* Question 6.1 *)
(****************)

(*
Exemples d'agrandissement d'images
 *)

(*
Sur la question 5, on peut voir un exemple d'agrandissement de l'image : On change tout simplement la valeur de k correspondant à l'agrandissement voulu
*)

(*Pour un image 2 fois plus grande, on multiplie la taille par 2 (soit k) *)

let size = 256;;

open_graph "" ;;
resize_window size size;;
draw_tree size arb_img_test1 ;;

(*Image aggrandi 2 fois*)
let dsize = 2*size;;
resize_window dsize dsize;;
draw_tree dsize arb_img_test1 ;;

close_graph()
;;

(****************)
(* Question 6.2 *)
(****************)
let rec rotation arb =
  match arb with
| Feuille _ -> arb
| Noeud (c1,c2,c3,c4) ->
   Noeud (rotation c3,rotation c1, rotation c4, rotation c2);;

(*
Exemples d'utilisation 
de la fonction rotation

*)

(**************)
(* Question 7 *)
(**************)
let rec fractale n =
  if n <= 0 then Feuille Noir
  else
     let c = fractale (n-1) in
     let c1 = Noeud (c,c,c,Feuille Blanc) in
     let c3 = rotation c1 in
     let c4 = rotation c3 in
     let c2 = rotation c4 in
     Noeud (c1,c2,c3,c4)
;;
(* 
Exemples d'utilisation 
de la fonction fractale

*)

(****************)
(* Question 8.1 *)
(****************)
let rec arbre_vers_bit a = match a with
| Feuille Blanc -> [0 ; 0]
| Feuille Noir  -> [0 ; 1 ]
| Noeud (a1,a2,a3,a4) ->
    1::arbre_vers_bit a1 @
    arbre_vers_bit a2 @
    arbre_vers_bit a3 @
    arbre_vers_bit a4
;;

(* 
Exemples d'utilisation 
de la fonction arbre_vers_bits
*)
arbre_vers_bit(arb_img_test1);;
*)

(****************)
(* Question 8.2 *)
(****************)

let rec sum(ilsb ,ln) =
  match ln with
  |[] -> 0
  |a :: res -> a * int_of_float(2. ** float_of_int(ilsb)) + sum(ilsb + 1, res)
;;


let rec complete(ln) =
  let size = List.length(ln) in
  match size mod 8 with
  |0 -> ln
  |a -> complete (ln @ [0])
;;

let rec bits_vers_octets lb =
  match complete lb with 
  |a0::a1::a2::a3::a4::a5::a6::a7::suiv -> [sum(0, [a0;a1;a2;a3;a4;a5;a6;a7])] @ bits_vers_octets suiv
  |_ -> []
;;



(* 
Exemples d'utilisation 
de la fonction bits_vers_octets

*)

bits_vers_octets [0;1;0;1;0;1;1;0;
                  0;1;0;1;0;1;1;1;
                  1;0;1;0;0;0];; (*Pas un multiple de 8*)

(****************)
(* Question 8.3 *)
(****************)

let rec bit_vers_arbre lb =
  match lb with
| 0::0::suiv -> Feuille Blanc, suiv
| 0::1::suiv -> Feuille Noir, suiv
| 1::suiv ->
    let a1,suiv = bit_vers_arbre suiv in
    let a2,suiv = bit_vers_arbre suiv in
    let a3,suiv = bit_vers_arbre suiv in
    let a4,suiv = bit_vers_arbre suiv in
    Noeud (a1,a2,a3,a4) , suiv

|_-> failwith "Erreur dans la forme de la liste"
;;
(* 
Exemples d'utilisation 
de la fonction bits_vers_arbres
*)



bit_vers_arbre [1;0;0;1;0;0;0;0;1];;

(*
Explications sur la gestion 
des erreurs de la fonction bits_vers_arbres
*)


let rec octet_vers_bits lo= 
  let rec binary value =
    if value=0 then []
    else value mod 2 :: binary(value / 2)
  in
  match lo with
  |[] -> []
  |a :: suiv -> complete(binary(a)) @ octet_vers_bits(suiv)
;;



(* 
Exemples d'utilisation 
de la fonction octet_vers_bits

*)

octet_vers_bits(bits_vers_octets [0;1;0;1;0;1;1;0;
                  0;1;0;1;0;1;1;1;
                  1;0;1;0;0;0]);;

(****************)
(* Question 8.4 *)
(****************)
(*
 let write_arbre filename arb =
*)
(* 
Exemples d'utilisation 
de la fonction write_arbre

*)

(*
let read_arbre filename =
*) 
(* 
Exemples d'utilisation 
de la fonction read_arbre

*)




