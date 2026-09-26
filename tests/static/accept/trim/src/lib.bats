#use array as A
#use str as S

(* Both results are proven in [0, n]: the trimmed length is a nat and
   the first kept byte can be read after a single refining test. *)
#pub fn first_kept {l:agz} (bv: !$A.borrow(byte, l, 8)): int

implement first_kept (bv) = let
  val a = $S.trim_left(bv, 8)
in if a < 8 then byte2int0($A.read<byte>(bv, a)) else ~1 end

#pub fn kept_end {l:agz} (bv: !$A.borrow(byte, l, 8)): [r:nat | r <= 8] int r

implement kept_end (bv) = $S.trim_right(bv, 8)
