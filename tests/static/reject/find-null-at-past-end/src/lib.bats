#include "share/atspre_staload.hats"
#use array as A
#use str as S

(* Start position beyond the buffer: must not type-check. *)
#pub fn past_end {l:agz} (buf: !$A.arr(byte, l, 8)): int

implement past_end (buf) = $S.find_null_at(buf, 9, 8)
