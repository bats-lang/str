#use array as A
#use str as S

(* An unindexed length carries no bound. *)
#pub fn unproven {l,lp:agz} (name: !$A.arr(byte, l, 16), len: int, ext: !$A.borrow(byte, lp, 5)): bool

implement unproven (name, len, ext) = $S.has_suffix(name, len, 16, ext, 5)
