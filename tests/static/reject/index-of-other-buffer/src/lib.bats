#use array as A
#use str as S

(* An index proven < 8 says nothing about a 4-byte buffer. *)
#pub fn wrong_buffer {l,m:agz} (bv: !$A.borrow(byte, l, 8), small: !$A.borrow(byte, m, 4)): int

implement wrong_buffer (bv, small) =
  case+ $S.index_of(bv, 8, 58) of
  | ~$S.str_some(i) => byte2int0($A.read<byte>(small, i))
  | ~$S.str_none() => ~1
