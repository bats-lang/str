#use array as A
#use str as S

(* A length past the buffer end would read out of bounds. *)
#pub fn too_long {l,lp:agz} (name: !$A.arr(byte, l, 16), ext: !$A.borrow(byte, lp, 5)): bool

implement too_long (name, ext) = $S.has_suffix(name, 17, 16, ext, 5)
