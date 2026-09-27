#use array as A
#use str as S

(* A source longer than the array must not type-check: fill_exact used
   to stop silently at the end of the array. *)
#pub fn long {l,lb:agz} (arr: !$A.arr(byte, l, 4), src: !$A.borrow(byte, lb, 6)): void

implement long (arr, src) = $S.fill_exact(arr, src, 4, 6, 0)
