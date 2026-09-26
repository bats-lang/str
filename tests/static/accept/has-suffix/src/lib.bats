#use array as A
#use str as S

(* A length proven <= the buffer size (here: a NUL search result). *)
#pub fn is_dats {l,lp:agz} (name: !$A.arr(byte, l, 16), ext: !$A.borrow(byte, lp, 5)): bool

implement is_dats (name, ext) = let
  val len = $S.find_null_at(name, 0, 16)
in $S.has_suffix(name, len, 16, ext, 5) end
