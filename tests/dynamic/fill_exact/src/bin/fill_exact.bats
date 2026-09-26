#include "share/atspre_staload.hats"
#use array as A
#use str as S

(* fill_exact copies src[i..] to arr[i..] up to the shorter end.
   Exits 1 on any mismatch. *)
fn same {l,lw:agz}{n:pos} (name: string, arr: !$A.arr(byte, l, n), n: int n, want: !$A.borrow(byte, lw, n)): bool = let
  val ok = $S.match_at_arr(arr, 0, want, n)
  val () = (if ok then () else println! ("FAIL ", name))
in ok end

implement main0 () = let
  var dots = @[char][4]('.', '.', '.', '.')
  var src3 = @[char][3]('a', 'b', 'c')
  var src6 = @[char][6]('u', 'v', 'w', 'x', 'y', 'z')
  var w1 = @[char][4]('a', 'b', 'c', '.')
  var w2 = @[char][4]('u', 'v', 'w', 'x')
  var w3 = @[char][4]('.', 'b', 'c', '.')
  val @(f3, s3) = $A.freeze<byte>($S.from_char_array(src3, 3))
  val @(f6, s6) = $A.freeze<byte>($S.from_char_array(src6, 6))
  val a1 = $S.from_char_array(dots, 4)
  val () = $S.fill_exact(a1, s3, 4, 3, 0)
  val @(fw1, bw1) = $A.freeze<byte>($S.from_char_array(w1, 4))
  val r1 = same("short src", a1, 4, bw1)
  val a2 = $S.from_char_array(dots, 4)
  val () = $S.fill_exact(a2, s6, 4, 6, 0)
  val @(fw2, bw2) = $A.freeze<byte>($S.from_char_array(w2, 4))
  val r2 = same("long src", a2, 4, bw2)
  val a3 = $S.from_char_array(dots, 4)
  val () = $S.fill_exact(a3, s3, 4, 3, 1)
  val @(fw3, bw3) = $A.freeze<byte>($S.from_char_array(w3, 4))
  val r3 = same("start 1", a3, 4, bw3)
  val () = $A.free<byte>(a1) val () = $A.free<byte>(a2) val () = $A.free<byte>(a3)
  val () = $A.drop<byte>(f3, s3) val () = $A.free<byte>($A.thaw<byte>(f3))
  val () = $A.drop<byte>(f6, s6) val () = $A.free<byte>($A.thaw<byte>(f6))
  val () = $A.drop<byte>(fw1, bw1) val () = $A.free<byte>($A.thaw<byte>(fw1))
  val () = $A.drop<byte>(fw2, bw2) val () = $A.free<byte>($A.thaw<byte>(fw2))
  val () = $A.drop<byte>(fw3, bw3) val () = $A.free<byte>($A.thaw<byte>(fw3))
in
  if r1 && r2 && r3 then println! ("fill_exact: all cases pass")
  else exit_void(1)
end
