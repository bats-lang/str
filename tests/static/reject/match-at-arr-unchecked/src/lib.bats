#use array as A
#use str as S

(* p is only known to be a nat, not to leave room for the pattern. *)
#pub fn unchecked {l,lp:agz}{p:nat}
  (src: !$A.arr(byte, l, 8), p: int p, pat: !$A.borrow(byte, lp, 3)): bool

implement unchecked (src, p, pat) = $S.match_at_arr(src, p, pat, 3)
