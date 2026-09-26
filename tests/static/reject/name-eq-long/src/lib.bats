#use array as A
#use str as S

(* A length past the buffer end would read out of bounds. *)
#pub fn too_long {l,lp:agz} (name: !$A.arr(byte, l, 16), want: !$A.borrow(byte, lp, 3)): bool

implement too_long (name, want) = $S.name_eq(name, 20, 16, want, 3)
