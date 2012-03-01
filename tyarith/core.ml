open Format
open Syntax
open Support.Error
open Support.Pervasive

(* ------------------------   EVALUATION  ------------------------ *)

exception NoRuleApplies

let rec isnumericval t = match t with
    TmZero(_) -> true
  | TmSucc(_,t1) -> isnumericval t1
  | TmPred(_,t1) -> isnumericval t1
  | _ -> false

let rec isval t = match t with
    TmTrue(_)  -> true
  | TmFalse(_) -> true
  | t when isnumericval t  -> true
  | _ -> false

let rec eval1 t = match t with
    TmIf(_,TmTrue(_),t2,t3) ->
      t2
  | TmIf(_,TmFalse(_),t2,t3) ->
      t3
  | TmIf(fi,t1,t2,t3) ->
      let t1' = eval1 t1 in
      TmIf(fi, t1', t2, t3)
  | TmSucc(_,TmPred(_,nv1)) when (isnumericval nv1) ->
      nv1
  | TmSucc(fi,t1) ->
      let t1' = eval1 t1 in
      TmSucc(fi, t1')
(*
  | TmPred(_,TmZero(_)) ->
      TmZero(dummyinfo)
*)
  | TmPred(_,TmSucc(_,nv1)) when (isnumericval nv1) ->
      nv1
  | TmPred(fi,t1) ->
      let t1' = eval1 t1 in
      TmPred(fi, t1')
  | TmIsZero(_,TmZero(_)) ->
      TmTrue(dummyinfo)
  | TmIsZero(_,TmSucc(_,TmPred(_, TmZero(_)))) ->
      TmTrue(dummyinfo)
  | TmIsZero(_,TmPred(_,TmSucc(_, TmZero(_)))) ->
      TmTrue(dummyinfo)
  | TmIsZero(_,nv1) when (isnumericval nv1) ->
      TmFalse(dummyinfo)
  | TmIsZero(fi,t1) ->
      let t1' = eval1 t1 in
      TmIsZero(fi, t1')
  | TmFst (_, TmPair (fi,v1,v2)) when (isval v1) && (isval v2)->
      v1
  | TmSnd (_, TmPair (fi,v1,v2)) when (isval v1) && (isval v2)->
      v2
  | TmFst (fi, t1) ->
      let t1' = eval1 t1 in
      TmFst(fi, t1')
  | TmSnd (fi, t1) ->
      let t1' = eval1 t1 in
      TmSnd(fi, t1')
(*
  | TmAnd(fi,TmBzero(_),TmBzero(_)) ->
  	TmBzero(dummyinfo)
  | TmAnd(fi,TmBzero(_),TmBone(_)) ->
  	TmBzero(dummyinfo)
  | TmAnd(fi,TmBone(_),TmBzero(_)) ->
  	TmBzero(dummyinfo)
  | TmAnd(fi,TmBone(_),TmBone(_)) ->
  	TmBone(dummyinfo)
  | TmAnd(fi,t1,t2) ->
  	let t1' = eval1 t1 and t2' = eval1 t2 in
  	TmAnd (fi,t1',t2')
*)
  | TmIncr(_,TmZZ(_)) ->
      TmZO(dummyinfo)
  | TmIncr(_,TmZO(_)) ->
      TmOZ(dummyinfo)
  | TmIncr(_,TmOZ(_)) ->
      TmOO(dummyinfo)
  | TmIncr(_,TmOO(_)) ->
      TmOO(dummyinfo)
  | TmIncr(fi,t1) ->
      let t1' = eval1 t1 in
      TmIncr(fi, t1')
  | _ -> 
      raise NoRuleApplies

let rec eval t =
  try let t' = eval1 t
      in eval t'
  with NoRuleApplies -> t

(* ------------------------   TYPING  ------------------------ *)

let rec typeof t =
  match t with
    TmTrue(fi) -> 
      TyBool
  | TmFalse(fi) -> 
      TyBool
(*
  | TmBzero(fi) ->
  	TyBin
  | TmBone(fi) ->
  	TyBin
*)
  | TmZZ(fi) -> 
      TyBin
  | TmZO(fi) -> 
      TyBin
  | TmOZ(fi) -> 
      TyBin
  | TmOO(fi) -> 
      TyBin
(* ADDED PAIR TYPE *)
  | TmPair(fi,v1,v2)   -> 
      if (isval v1) && (isval v2) then
         let tyT1 = typeof v1 and tyT2 = typeof v2 in
         if (=) tyT1 tyT2 then
           TyPair (tyT1, tyT2)
         else
           error fi "arms of pair have different types"
      else
         error fi "pair not made up of values"
  | TmFst (_,TmPair(fi,t1,t2)) ->
      if (=) (typeof (TmPair(fi,t1,t2))) (TyPair ((typeof t1),(typeof t2))) then
        typeof t1
      else
        error fi "argument not a pair"
  | TmSnd (_,TmPair(fi,t1,t2)) ->
      if (=) (typeof (TmPair(fi,t1,t2))) (TyPair ((typeof t1),(typeof t2))) then
        typeof t2
      else
        error fi "argument not a pair"
  | TmFst (fi,_) ->
      error fi "argument not a pair"
  | TmSnd (fi,_) ->
      error fi "argument not a pair"
  | TmIf(fi,t1,t2,t3) ->
     if (=) (typeof t1) TyBool then
       (*
       let tyT2 = typeof t2 in
       if (=) tyT2 (typeof t3) then tyT2
       else error fi "arms of conditional have different types"
       *)
       (* ALLOWS DIFFERENT TYPES IN IF-ELSE *)
       let tyT1 = typeof t2 and tyT2 = typeof t3 in
       TyIf (tyT1, tyT2)
     else error fi "guard of conditional not a boolean"
  | TmZero(fi) ->
      TyNat
  | TmSucc(fi,t1) ->
      if (=) (typeof t1) TyNat then TyNat
      else error fi "argument of succ is not a number"
  | TmPred(fi,t1) ->
      if (=) (typeof t1) TyNat then TyNat
      else error fi "argument of pred is not a number"
  | TmIsZero(fi,t1) ->
      if (=) (typeof t1) TyNat then TyBool
      else error fi "argument of iszero is not a number"
  | TmAnd(fi,t1,t2) ->
  	if (=) (typeof t1) TyBin && (=) (typeof t2) TyBin then TyBin
  	else error fi "argument of and are not binary"
  | TmIncr(fi,t1) ->
      if (=) (typeof t1) TyBin then TyBin
      else error fi "argument of incr is not a binary"
