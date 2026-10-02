#include "share/atspre_staload.hats"
#use array as A
#use str as S

(* An unindexed length carries no bound. *)
#pub fn unproven {l,lp:agz} (name: !$A.arr(byte, l, 16), len: int, want: !$A.borrow(byte, lp, 3)): bool

implement unproven (name, len, want) = $S.name_eq(name, len, 16, want, 3)
