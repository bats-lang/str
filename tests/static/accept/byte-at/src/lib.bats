#include "share/atspre_staload.hats"
#use array as A
#use str as S

(* A loop whose index is proven in range reads every byte without a
   runtime check. *)
#pub fn sum_bytes {l:agz}{n:pos} (bv: !$A.borrow(byte, l, n), n: int n): int

implement sum_bytes {l}{n} (bv, n) = let
  fun loop {i:nat | i <= n} .<n - i>.
    (bv: !$A.borrow(byte, l, n), n: int n, i: int i, acc: int): int =
    if i >= n then acc else loop(bv, n, i + 1, acc + $S.byte_at(bv, i))
in loop(bv, n, 0, 0) end
