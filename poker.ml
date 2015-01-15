(* project euler 54*)

type suit = C | D | H | S
type cardval = Val of int | Jack | Queen | King | Ace
type card = suit * cardval
type rank = Highest of card | Pair of card | TwoPairs of (card*card) | Three of card | Straight of card (* only the highest card *)
	    | Flush of (card list) (* only the highest card *) | FullHouse of card * card (* Three, Two *) | Four of card | StraightFlush of card
	    | RoyalFlush

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

let testConsecutive (game : card list) = List.for_all2 (fun x y -> try (snd y = next (snd x)) with _ -> false) (List.rev (List.tl (List.rev game))) (List.tl game);;

assert(testConsecutive [C,Val 10;D, Jack;S,Queen]);;
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
  | t1::t2::t3::p1::[p2] when (vl t1 = vl t2 && vl t1 = vl t3 && vl p1 = vl p2) -> Some (FullHouse(t1,p1))
  | p1::p2::t1::t2::[t3] when (vl t1 = vl t2 && vl t1 = vl t3 && vl p1 = vl p2) -> Some (FullHouse(t1,p1))
  | _ -> None;;

testFull [C,Jack;D,Jack;H,Jack;S,Jack;C,Val 10];;
testFull [C,Jack;D,Jack;S,Val 10;H,Jack;C,Val 10];;

let testFlush (game : card list) = if testSameSuit game then Some(Flush (sortReverseHighest compCards game)) else None;;

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

let rankVal = function
  | Highest _ -> 1
  | Pair c1 -> 2
  | TwoPairs (c1,c2) -> 3
  | Three c1 -> 4
  | Straight c -> 5 
  | Flush c -> 6
  | FullHouse (c1,c2) -> 7 (* Three, Two *)
  | Four c -> 8
  | StraightFlush c -> 9
  | RoyalFlush -> 10;;


let compareGames game1 game2 =
  let r1 = rankVal (getRank game1)
  and r2 = rankVal (getRank game2) in
  if r1 < r2 then 1 else if r2 < r1 then 2 else
      3;;
  




(* try *)
(*  while true do *)
(*    let line = input_line stdin in *)
(*    treat line *)
(*  done; *)
(*  None *)
(* with *)
(*  End_of_file -> None *)
(* ;; *)
