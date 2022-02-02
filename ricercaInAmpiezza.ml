exception SoluzioneNonTrovata;;

type 'a graph = Graph of ('a -> 'a list);;

let rec stampalista = function 
	|[] -> print_newline()
	|(x,y)::rest -> 
  	print_string("(");
  	print_char(Char.chr (x + 64));
  	print_string(",");
  	print_int(y); 
  	print_string(")");
  	print_string("; "); stampalista rest
;;

(** Spostamenti possibili di un cavallo su una scacchiera
* sia (x,y) la posizione attuale del cavallo, le nuove posizioni possibili sono (x + dx, y + dy) per ogni (dx, dy) in deltaSalti
* che siano all'interno della scacchiera
*)
let deltaSalti = [(1,2);(1,-2);(-1,2);(-1,-2);(2,1);(2,-1);(-2,1);(-2,-1)];;

(** Una posizione (x, y) é sicura se si trova all'interno della scacchiera *)
let posizioneSicura (x,y) n = x > 0 && x <= n && y > 0 && y <= n;;

let time f x =
  	let t = Sys.time() in
  	let fx = f x in
  	Printf.printf "execution time: %fs\n" (Sys.time() -. t);
  	fx
;;

(** La funzione successori, data una posizione, restituisce la lista delle nuove possibili posizioni sicure dopo un salto *)
let successori n stato =
  	let rec successoriAux risultato = function
    	|[] -> risultato
    	|(dx,dy)::rest -> 
      		let x = (fst stato) + dx in
      		let y = (snd stato) + dy in
      		if posizioneSicura (x,y) n then
        		successoriAux ((x,y)::risultato) rest
      		else
        		successoriAux risultato rest
  	in successoriAux [] deltaSalti
;;

(** camminoSuccessore prende in input un cammino (sequenza di salti) e restituisce la lista dei cammini possibili dopo un salto *)
let camminoSuccessore successori cammino =
  	let rec camminoSuccessoreAux = function
    	|[] -> []
    	|h::t -> 
      		(** qui viene controllato che la nuova posizione non sia giá presente nel cammino *)
      		if List.mem h cammino then
        	camminoSuccessoreAux t
      	else
        	(h::cammino)::(camminoSuccessoreAux t)
  	in camminoSuccessoreAux (successori (List.hd cammino))
;;

(** visitaAmpiezza visita semplicemente in ampiezza (BFS) il grafo generato da camminoSuccessore, quindi il grafo dei cammini *)
let visitaAmpiezza (Graph successori) obiettivo inizio =
  	let rec cerca = function
    	|[] -> raise SoluzioneNonTrovata
    	|cammino::resto ->
      		if obiettivo cammino then 
        		cammino
      		else cerca (resto @ (successori cammino))
  	in cerca [inizio]
;;

(** Il goal dell'algoritmo é trovare una sequenza di spostamenti lunga quanto la grandezza della scacchiera
* piú un'ultimo salto che faccia tornare il cavallo alla posizione di partenza
* la funzione di visita avrá il compito di controllare che queste posizioni non siano ripetute
*)
let obiettivo n grandezzaScacchiera inizio risultato = 
  (List.length risultato) = grandezzaScacchiera && (List.exists ((=) inizio) (successori n (List.hd risultato)))
;;
  
let cercaCamminoInAmpiezza ((Graph successori), obiettivo, inizio) =
  List.rev (visitaAmpiezza (Graph (camminoSuccessore successori)) obiettivo [inizio])
;;

let saltoDelCavallo inizio n =
  stampalista (cercaCamminoInAmpiezza ((Graph (successori n)), (obiettivo n (n * n) inizio), inizio)) 
;;

let t = Sys.time();;
saltoDelCavallo (1,1) 6;;
Printf.printf "execution time: %fs\n" (Sys.time() -. t);;