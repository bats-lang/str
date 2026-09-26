#use array as A
#use str as S

(* r + 2 can be 3: out of a 3-element range, must not type-check. *)
#pub fn bad_index {la,lb:agz}
  (a: !$A.borrow(byte, la, 4), b: !$A.borrow(byte, lb, 4)): [i:nat | i < 3] int i

implement bad_index (a, b) = $S.compare(a, 4, b, 4) + 2
