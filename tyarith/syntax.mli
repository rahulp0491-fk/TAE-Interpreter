(* module Syntax: syntax trees and associated support functions *)

open Support.Pervasive
open Support.Error

(* Data type definitions *)
type ty =
    TyBool
  | TyNat
  | TyIf of ty * ty
  | TyPair of ty * ty
  | TyBin

type term =
    TmTrue of info
  | TmFalse of info
  | TmIf of info * term * term * term
  | TmZero of info
  | TmSucc of info * term
  | TmPred of info * term
  | TmIsZero of info * term
  | TmPair of info * term * term
  | TmFst of info * term
  | TmSnd of info * term
  | TmAnd of info * term * term
  | TmBzero of info
  | TmBone of info
  | TmIncr of info * term
  | TmZZ of info
  | TmZO of info
  | TmOZ of info
  | TmOO of info

type command =
  | Eval of info * term



(* Printing *)
val printtm: term -> unit
val printtm_ATerm: bool -> term -> unit
val printty : ty -> unit

(* Misc *)
val tmInfo: term -> info

