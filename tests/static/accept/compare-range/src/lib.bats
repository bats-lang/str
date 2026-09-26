#use array as A
#use str as S

(* compare's result is statically in [-1, 1], so r + 1 indexes a
   3-element table without any check. *)
#pub fn order_index {la,lb:agz}
  (a: !$A.borrow(byte, la, 4), b: !$A.borrow(byte, lb, 4)): [i:nat | i < 3] int i

implement order_index (a, b) = $S.compare(a, 4, b, 4) + 1
