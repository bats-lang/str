#include "share/atspre_staload.hats"
#use array as A
#use str as S

(* An unindexed position carries no bound: must not type-check. *)
#pub fn at {l:agz} (bv: !$A.borrow(byte, l, 4), p: int): int

implement at (bv, p) = $S.byte_at(bv, p)
