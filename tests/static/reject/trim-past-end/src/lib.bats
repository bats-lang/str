#use array as A
#use str as S

(* trim_left can return n itself, so reading at it unchecked is out of bounds. *)
#pub fn unchecked {l:agz} (bv: !$A.borrow(byte, l, 8)): int

implement unchecked (bv) = byte2int0($A.read<byte>(bv, $S.trim_left(bv, 8)))
