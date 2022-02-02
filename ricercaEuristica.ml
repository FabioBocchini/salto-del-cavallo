exception SoluzioneNonTrovata;;

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

let time f x =
    let t = Sys.time() in
    let fx = f x in
    Printf.printf "execution time: %fs\n" (Sys.time() -. t);
    fx
;;

let saltoDelCavallo (inizio, n) = 

    (** Spostamenti possibili di un cavallo su una scacchiera
    * sia (x,y) la posizione attuale del cavallo, le nuove posizioni possibili sono (x + dx, y + dy) per ogni (dx, dy) in deltaSalti
    * che siano all'interno della scacchiera
    *)
    let deltaSalti = [(1,2);(1,-2);(-1,2);(-1,-2);(2,1);(2,-1);(-2,1);(-2,-1)] in

    (** Una posizione (x, y) é sicura se si trova all'interno della scacchiera *)
    let posizioneSicura (x,y) n = x > 0 && x <= n && y > 0 && y <= n in


    (** La funzione successori, data una posizione, restituisce la lista delle nuove possibili posizioni sicure dopo un salto *)
    let successori stato =
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
    in
 
    (** La funzione estendi, dato un cammino, restituisce la lista di cammini formati dal cammino input più uno dei salti possibili
    * la cui casella non sia già presente nel cammino di input
    *)
    let estendi cammino =
        List.map (fun x -> x::cammino) (List.filter (fun x -> not(List.mem x cammino)) (successori (List.hd cammino)))
    in

    (** warndorff è una funzione di ordinamento che, date due posizioni p1 e p2, restituisce 1, 0 o -1, in base alla funzione euristica
    *  di Warndorff. L'euristica di Warnorff preferisce posizioni in cui il cavallo ha meno salti possibili.
    *)
    let warnsdorff p1 p2 = 
        let a = List.length(List.filter (fun x -> not(List.mem x p1)) (successori (List.hd p1))) in
        let b = List.length(List.filter (fun x -> not(List.mem x p2)) (successori (List.hd p2))) in    

        if a>b then 1
        else if a=b then 0
        else -1 
    in    

    (** Il goal dell'algoritmo é trovare una sequenza di spostamenti lunga quanto la grandezza della scacchiera
    * piú un'ultimo salto che faccia tornare il cavallo alla posizione di partenza
    *)
    let obiettivo risultato = 
        (List.length risultato) = (n*n) && (List.exists ((=) inizio) (successori (List.hd risultato)))
    in

    let rec cerca  = function
        |[] -> raise SoluzioneNonTrovata
        |cammino::resto ->
            if obiettivo cammino then 
                stampalista (List.rev cammino)
            else 
                cerca((List.sort warnsdorff (estendi cammino)) @ resto)
    in
        
    cerca [[inizio]]
;;

let t = Sys.time();;
saltoDelCavallo ((4,4), 8);;
Printf.printf "execution time: %fs\n" (Sys.time() -. t);;