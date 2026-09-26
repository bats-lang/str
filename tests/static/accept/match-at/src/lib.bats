#use array as A
#use str as S

(* Where the fit is unknown, a refining p + 3 <= 8 test proves it. *)
#pub fn matches_at {l,lp:agz}{p:nat}
  (src: !$A.borrow(byte, l, 8), p: int p, pat: !$A.borrow(byte, lp, 3)): bool

implement matches_at (src, p, pat) =
  if p + 3 <= 8 then $S.match_at(src, p, pat, 3) else false

(* A fit known statically needs no test at all. *)
#pub fn matches_tail {l,lp:agz} (src: !$A.arr(byte, l, 8), pat: !$A.borrow(byte, lp, 3)): bool

implement matches_tail (src, pat) = $S.match_at_arr(src, 5, pat, 3)
