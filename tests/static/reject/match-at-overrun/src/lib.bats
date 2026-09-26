#use array as A
#use str as S

(* 6 + 3 > 8: the pattern would run past the end. *)
#pub fn overrun {l,lp:agz} (src: !$A.borrow(byte, l, 8), pat: !$A.borrow(byte, lp, 3)): bool

implement overrun (src, pat) = $S.match_at(src, 6, pat, 3)
