#use array as A
#use str as S

(* The result is statically within [p, n]. *)
#pub fn first_nul {l:agz} (buf: !$A.arr(byte, l, 8)): [r:int | 0 <= r; r <= 8] int r

implement first_nul (buf) = $S.find_null_at(buf, 0, 8)

(* After a g1 comparison the result indexes the buffer: no cast needed. *)
#pub fn byte_at_nul {l:agz} (buf: !$A.arr(byte, l, 8)): int

implement byte_at_nul (buf) = let
  val r = $S.find_null_at(buf, 2, 8)
in
  if r < 8 then byte2int0($A.get<byte>(buf, r)) else ~1
end

#pub fn first_nul_bv {l:agz} (bv: !$A.borrow(byte, l, 8)): [r:int | 3 <= r; r <= 8] int r

implement first_nul_bv (bv) = $S.find_null_bv_at(bv, 3, 8)
