(*******************************
       Projet AP3

   Pr�nom :
   Nom :

********************************)


(*********************************
  Mise en place de la biblioth�que
  graphique
********************************)

(*
   on charge la bilbioth�que dans l'interpr�teur
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
  Dessine une image donn�e comme un tableau de couleurs
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
	  failwith "ce n'est pas une image carr�e"
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
       Mettez ici vos r�ponses
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
  forloop((t,img), n, (function(t,img) -> let i = Random.int(t) in
                                          let j = Random.int(t) in
                                          img.(i).(j) <- Noir;
                                          (t,img)));
img
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

    +-----+-----+
    | nw  | ne  |
    |     |     |
W-  +-----+-----+  -E
    | sw  | se  |
    |     |     |
    +-----+-----+

          |
          S

D�coupage en r�gion de l'image de taille kn
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
  snd (forloop((0, img),
               k-i+1,
               (function (a,img) -> snd (forloop((j, ()),
                                                k-j,
                                                (function (b, ()) -> print_int a;
                                                                     img.(a).(b) <- Noir;
					                             print_string " ";
					                             print_int b;
					                             print_newline (); (b+1, ()))));
                                   (a+1, img)))
    )
;;

(* 
Exemples d'utilisation q
de la fonction remplir_carre
*)
remplir_carre(random_img(4,0), 0,2,4, Noir);;

let  arbre_vers_image(k,arb : int * arbre) : picture =
  let img = Array.make_matrix k k Blanc in
  let rec fill_image(i, j, k, arb : int * int * int * arbre) : picture =
    match arb with
    |Feuille c -> remplir_carre(img, i, j, k, c)
    |Noeud (nw, ne, sw, se) ->
      let kn = k/2 in
      fill_image(i, (j+kn), kn, ne);
      fill_image(i,j, kn, nw);
      fill_image((i+kn), (j+kn), kn, se);
      fill_image((i+kn), j, kn, sw)
  in
  fill_image(0, 0, k, arb)
;;

(*
Exemples d'utilisation 
de la fonction arbre_vers_image
*)

arbre_vers_image(4,image_vers_arbre(4, img_test1));; (* Ne marche pas ?*)

(**************)
(* Question 5 *)
(**************)
(*
let draw_tree k arb =
*)
(* 
Exemples d'utilisation 
de la fonction draw_tree

*)

(****************)
(* Question 6.1 *)
(****************)

(*
Exemples d'agrandissement d'images

*)

(****************)
(* Question 6.2 *)
(****************)

(*
let rotation arb = 
*)
(* 
Exemples d'utilisation 
de la fonction rotation

*)

(**************)
(* Question 7 *)
(**************)
(*
let fractale k n =
*)
(* 
Exemples d'utilisation 
de la fonction fractale

*)

(****************)
(* Question 8.1 *)
(****************)
(*
let arbre_vers_bits arb =
*)
(* 
Exemples d'utilisation 
de la fonction arbre_vers_bits

*)

(****************)
(* Question 8.2 *)
(****************)
(*
let bits_vers_octets lb =
*)
(* 
Exemples d'utilisation 
de la fonction bits_vers_octets

*)

(****************)
(* Question 8.3 *)
(****************)
(*
let bits_vers_arbres lb =
*)
(* 
Exemples d'utilisation 
de la fonction bits_vers_arbres

*)
(*
Explications sur la gestion 
des erreurs de la fonction bits_vers_arbres
*)

(*
let octet_vers_bits lo =
*)
(* 
Exemples d'utilisation 
de la fonction octet_vers_bits

*)

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




