#include "share/atspre_staload.hats"
#use array as A
#use str as S

(* Fewer than 11 bytes of room must not type-check: int_to_str used to
   return pos unchanged, writing nothing, when the number did not fit. *)
#pub fn cramped {l:agz} (buf: !$A.arr(byte, l, 16)): int

implement cramped (buf) = $S.int_to_str(buf, 6, 16, 42)
