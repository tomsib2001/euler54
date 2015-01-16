(* project euler 54*)

type suit = C | D | H | S
type cardval = Val of int | Jack | Queen | King | Ace
type card = suit * cardval

let suit_of_string = function
  | "C" -> C
  | "D" -> D
  | "H" -> H
  | "S" -> S
  | _ -> failwith "bad suit"
;;

let string_of_suit = function
  | C -> "Clubs"
  | D -> "Diamonds"
  | H -> "Hearts"
  | S -> "Spades"
;;

let val_of_string = function
  | "J" -> Jack
  | "Q" -> Queen
  | "K" -> King
  | "A" -> Ace 
  | "T" -> Val 10 
  | s -> if try (string_of_int (int_of_string s) = s) with _ -> false then (Val (int_of_string s)) else begin
    print_string s;
    failwith "not a valid card"
  end
;;

let string_of_val = function
  | Jack -> "Jack"
  | Queen -> "Queen"
  | King -> "King"
  | Ace -> "Ace" 
  | Val i -> string_of_int i
;;

	 
	 let card_of_string str = 
	   let s = String.make 1 (str.[1]) and
	       vl =  String.make 1 (str.[0]) in
	   ((suit_of_string s, val_of_string vl) : card);;

	 let string_of_card ((s,v) : card) = String.concat " of " [string_of_val v; string_of_suit s];;
	 

type rank = Highest of card | Pair of card | TwoPairs of (card*card) | Three of card | Straight of card (* only the highest card *)
	    | Flush of (card) (* only the highest card *) | FullHouse of card (* Three *) | Four of card | StraightFlush of card
	    | RoyalFlush

let string_of_rank = function
  | Highest c1 -> String.concat "" ["Highest (";string_of_card c1;")"]
  | Pair c1 -> String.concat "" ["Pair (";string_of_card c1;")"]
  | TwoPairs (c1,c2) -> String.concat "" ["TwoPairs (";string_of_card c1;" and ";string_of_card c2;")"]    
  | Three c1 -> String.concat "" ["Three (";string_of_card c1;")"]
  | Flush c1 -> String.concat "" ["Flush (";string_of_card c1;")"]
  | FullHouse c1 -> String.concat "" ["FullHouse (";string_of_card c1;")"]
  | Four c1 -> String.concat "" ["Four (";string_of_card c1;")"]
  | Straight c1 -> String.concat "" ["Straight (";string_of_card c1;")"]
  | StraightFlush c1 -> String.concat "" ["StraightFlush (";string_of_card c1;")"]
  | RoyalFlush -> "RoyalFlush";;

let st (c : card) = fst c;;
let vl (c : card) = snd c;;

let value = function
  | Val i when 2 <= i && i <= 10-> i
  | Jack -> 11
  | Queen -> 12
  | King -> 13
  | Ace -> 14
  | _ -> failwith "not a card";;

let sortReverseHighest comp l = 
  let rec get_max def l1 = function
    | [] -> (def,l1)
    | h::t -> if comp def h then get_max h (def::l1) t else get_max def (h::l1) t 
  in
  let rec aux = function
    | [] -> []
    | h::t -> let (m,l1) = get_max h [] t in m::(aux l1)
  in aux l;;
  
(* sortReverseHighest (<=) [3;2;5;1;7];; *)

let compCards (x : card) (y : card) = value (vl x) <= value (vl y);;
let strictCompCards (x : card) (y : card) = value (vl x) < value (vl y);;
let getWinner (x : card) (y : card) = if strictCompCards x y then 2 else (if vl x = vl y then (-1) else 1) (* -1 means no clear winner *)

let getHighestCard (game : card list) = List.hd (sortReverseHighest compCards game);;


let next = function
  | Val i when 2 <= i && i <= 9 -> Val (i+1)
  | Val 10 -> Jack
  | Jack -> Queen
  | Queen -> King
  | King -> Ace
  | Ace -> failwith "no successor"
  | _ -> failwith "not a card"
;; 

let testSameSuit (game : card list) = match game with
  | (s,cv)::t -> List.for_all (fun x -> fst x = s) t 
  | _ -> failwith "not a game"
;;

let testRoyalFlush (game : card list) = match game with
  | [] -> failwith "empty game"
  | (s,cv)::t -> if game = [(s,Val 10);(s,Jack);(s,Queen);(s,King);(s,Ace)] then Some RoyalFlush else None;;

let testConsecutive (game : card list) = 
  let game = List.rev (sortReverseHighest compCards game) in
  List.for_all2 (fun x y -> try (snd y = next (snd x)) with _ -> false) (List.rev (List.tl (List.rev game))) (List.tl game);;

assert(testConsecutive [C,Val 10;D, Jack;S,Queen]);;
assert(testConsecutive [C,Val 10;S,Queen;D, Jack]);;
assert(not (testConsecutive [C,Val 10;D, Jack;S,King]));;

let testStraightFlush (game : card list) = 
 if testSameSuit game && testConsecutive game then Some(StraightFlush (List.hd (sortReverseHighest compCards game))) else None;;

let count cardval (game : card list) = 
  let rec aux res = function
  | [] -> res
  | (s,cv)::t -> aux (if cv = cardval then res+1 else res) t
  in aux 0 game;;

(* count Jack [C,Val 10;D, Jack;S,King];; *)

let testFour (game : card list) = 
try Some (Four (List.find (fun x -> count (vl x) game = 4) game)) with _ -> None;;

testFour [C,Jack;D,Jack;H,Jack;S,Jack;C,Val 10];;


let testFull game = 
(* List.for_all (fun x -> fst x = s) t && *)
  let game1 = sortReverseHighest compCards game in
  match game1 with
  | t1::t2::t3::p1::[p2] when (vl t1 = vl t2 && vl t1 = vl t3 && vl p1 = vl p2) -> Some (FullHouse(t1))
  | p1::p2::t1::t2::[t3] when (vl t1 = vl t2 && vl t1 = vl t3 && vl p1 = vl p2) -> Some (FullHouse(t1))
  | _ -> None;;

testFull [C,Jack;D,Jack;H,Jack;S,Jack;C,Val 10];;
testFull [C,Jack;D,Jack;S,Val 10;H,Jack;C,Val 10];;

let testFlush (game : card list) = if testSameSuit game then Some(Flush (List.hd (sortReverseHighest compCards game))) else None;;

let testStraight (game : card list) = if testConsecutive game then Some(Straight(getHighestCard game)) else None;;

let testThree (game : card list) =   
try Some(Three(List.find (fun x -> count (vl x) game = 3) game)) with _ -> None;; 

let testTwoPairs (game : card list) =
  let res = ref None in
  if List.exists 
    (fun x -> 
      (count (vl x) game = 2) 
      && List.exists 
	(fun y -> 
	  (vl y) <> (vl x) 
	  && 
	    (
	      if count (vl y) game = 2 then 
		begin 
		  if compCards x y then 
		    res := Some (TwoPairs(y,x))
		  else (* y < x *)
		    res := Some (TwoPairs(x,y));
		  true 
		end else 
		false
	    )) game) 
    game then !res else None;;


let testPair (game : card list) =
  try Some (Pair (List.find (fun x -> count (vl x) game = 2) game)) with _ -> None;;


let getHighCard (game : card list) = (Highest (getHighestCard game));;

getHighCard [C,Jack;D,Jack;S,Val 10;H,Jack;C,Val 10];;

let listRankFuns =
[testRoyalFlush;  testStraightFlush; testFour; testFull; testFlush; testStraight;
testThree; testTwoPairs; testPair];;


let getRank game =
  let rec aux = function
    | [] -> getHighCard game
    | h::t -> (match (h game) with Some u -> u | None -> aux t)
	       in aux listRankFuns;;


getRank [C,Jack;D,Jack;S,Val 10;H,Jack;C,Val 10];;

let rankVal r = 
  (* let sg (\* sorted game *\)= sortReverseHighest compCards g in *)
  match r with
  | Highest c -> 1
  | Pair c -> 2
  | TwoPairs (c1,c2) -> 3
  | Three c1 -> 4
  | Straight c -> 5 
  | Flush c -> 6
  | FullHouse (c1) -> 7 (* Three, Two *)
  | Four c -> 8
  | StraightFlush c -> 9
  | RoyalFlush -> 10;;

type win = DiffRank | SameRank;;

let compareGames game1 game2 =
  let r1 = (getRank game1) and
      r2  = (getRank game2) in
  let rv1 = rankVal r1
  and rv2 = rankVal r2 in
  if rv1 < rv2 then (2,DiffRank) else if rv2 < rv1 then (1,DiffRank) else
      let res = match (r1,r2) with
	| Highest c1, Highest c2 -> getWinner c1 c2
	| Pair c1, Pair c2 -> getWinner c1 c2
	| TwoPairs (c1,c2),TwoPairs(c3,c4) ->
	  (match getWinner c1 c3 with
	  | -1 -> getWinner c2 c4
	  | k -> k)
	| Three c1, Three c2 -> getWinner c1 c2
	| Straight c1, Straight c2 -> getWinner c1 c2
	| Flush c1,Flush c2 -> getWinner c1 c2
	| FullHouse c1,FullHouse c2 -> getWinner c1 c2
	| Four c1, Four c2 -> getWinner c1 c2
	| StraightFlush c1, StraightFlush c2 -> getWinner c1 c2
	| RoyalFlush, RoyalFlush -> failwith "only clear winners in this game"
	| (_,_) -> failwith "ranks can't be different at this stage"
      in 
      (match res with
      | -1 -> 
	let rec aux = function
	  | ([],[]) -> failwith "only clear winners in this game"
	  | (h::t,h1::t1) -> let w =getWinner h h1 in if w = -1 then aux (t,t1) else (w,SameRank)
	  | (_,_) -> failwith "ill-formed game"
	in aux (sortReverseHighest compCards game1,sortReverseHighest compCards game2)
      | k -> (k,SameRank))
;;

(* let instances = 1000;; *)

(* let input() = *)
(*   for i = 0 to instances-1 do   *)
(*     let _ = input_line stdin in *)
(*     let t = Array.init 9 (fun _ -> input_line stdin) in *)
(*     let sudoku = Array.map (fun s -> Array.of_list (List.map (fun x -> int_of_char x - int_of_char '0') (explode s))) t in *)
    
(*   done;; *)

let extract k l = 
  let rec aux = function
    | (0,l1,l2) -> ((List.rev l1),l2)
    | (k,l1,h::t) -> aux (k-1,h::l1,t)
    | _ -> failwith "list not long enough"
  in aux (k,[],l);;

(* extract 3 [1;2;3;4;5;6];; *)
 
let split s = Str.split (Str.regexp " ") s;;

let evaluate_line line = 
  let l = split line in
  let (g1,g2) = extract 5 l in
  let (g1,g2) = (List.map card_of_string g1,List.map card_of_string g2) in
  (getRank g1,getRank g2);;

evaluate_line "2D 9C AS AH AC 3D 6D 7D TD QD";;
evaluate_line "2D 3D 4D 5D AD 3D 6D 7D TD QD";;

let treat_line line = 
  let l = split line in
  let (g1,g2) = extract 5 l in
  let (g1,g2) = (List.map card_of_string g1,List.map card_of_string g2) in
  compareGames g1 g2
;;
let res = ref 0;;

(* Str.split (Str.regexp " ")  "8C TS KC 9H 4S 7D 2S 5D 3S AC";; *)

evaluate_line "QS QD AC AD 4C 6S 2D AS 3H KC";;

(* evaluate_line "2D 9C AS AH AC 3D 6D 7D TD QD";; *)
(* treat_line "8C TS KC 9H 4S 7D 2S 5D 3S AC";; *)
try
 while true do
   let line = input_line stdin in
   let temp = treat_line line in
   let temp1 = evaluate_line line in
   res := !res + (if fst(temp)=1 then 1 else (if (fst temp) = 2 then  0 else failwith "coucou"));
   match snd temp with
   | SameRank -> (print_int (fst temp); print_string ": victory by same rank: ";print_string line; print_newline(); print_string (string_of_rank (fst temp1)); print_string "   vs.   "; print_string (string_of_rank (snd temp1)); print_newline())
   | DiffRank -> (print_int (fst temp); print_string ": victory by different rank "; print_string line; print_newline(); print_string (string_of_rank (fst temp1)); print_string "   vs.   "; print_string (string_of_rank (snd temp1)); print_newline())
 done;
 None
with
 End_of_file -> None
;;

print_newline();;
print_int !res;;
print_newline();;
