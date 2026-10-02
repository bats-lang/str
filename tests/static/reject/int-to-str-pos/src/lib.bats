#include "share/atspre_staload.hats"
#use array as A
#use str as S

(* A start position beyond the buffer must not type-check. *)
#pub fn past_end {l:agz} (buf: !$A.arr(byte, l, 16)): int

implement past_end (buf) = $S.int_to_str(buf, 17, 16, 42)
