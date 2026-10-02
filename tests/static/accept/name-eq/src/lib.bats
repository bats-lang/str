#include "share/atspre_staload.hats"
#use array as A
#use str as S

(* A length proven <= the buffer size (here: a NUL search result). *)
#pub fn is_src {l,lp:agz} (name: !$A.arr(byte, l, 16), want: !$A.borrow(byte, lp, 3)): bool

implement is_src (name, want) = let
  val len = $S.find_null_at(name, 0, 16)
in $S.name_eq(name, len, 16, want, 3) end
