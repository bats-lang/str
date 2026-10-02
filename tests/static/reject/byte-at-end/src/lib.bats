#include "share/atspre_staload.hats"
#use array as A
#use str as S

(* Index n is one past the end: must not type-check. *)
#pub fn last_plus_one {l:agz} (bv: !$A.borrow(byte, l, 4)): int

implement last_plus_one (bv) = $S.byte_at(bv, 4)
