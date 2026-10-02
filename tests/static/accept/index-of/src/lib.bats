#include "share/atspre_staload.hats"
#use array as A
#use str as S

(* The found index is proven < 8, so it reads the buffer directly. *)
#pub fn byte_after_colon {l:agz} (bv: !$A.borrow(byte, l, 8)): int

implement byte_after_colon (bv) =
  case+ $S.index_of(bv, 8, 58) of
  | ~$S.str_some(i) => if i + 1 < 8 then byte2int0($A.read<byte>(bv, i + 1)) else ~1
  | ~$S.str_none() => ~1
