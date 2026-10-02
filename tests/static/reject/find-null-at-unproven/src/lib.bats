#include "share/atspre_staload.hats"
#use array as A
#use str as S

(* An unindexed int carries no bound: must not type-check. *)
#pub fn unproven {l:agz} (buf: !$A.arr(byte, l, 8), p: int): int

implement unproven (buf, p) = $S.find_null_at(buf, p, 8)
