#include "share/atspre_staload.hats"
#use array as A
#use str as S

(* eq(a, b) is true exactly when lengths and all bytes match.
   Exits 1 on any mismatch. *)
fn check_case {na,nb:pos}{la,lb:agz}
  (name: string, a: $A.arr(byte, la, na), a_n: int na,
   b: $A.arr(byte, lb, nb), b_n: int nb, want: bool): bool = let
  val @(fa, ba) = $A.freeze<byte>(a)
  val @(fb, bb) = $A.freeze<byte>(b)
  val got = $S.eq(ba, a_n, bb, b_n)
  val () = $A.drop<byte>(fa, ba)
  val () = $A.free<byte>($A.thaw<byte>(fa))
  val () = $A.drop<byte>(fb, bb)
  val () = $A.free<byte>($A.thaw<byte>(fb))
  val ok = (got = want)
  val () = (if ok then () else println! ("FAIL ", name, ": got ", got, ", want ", want))
in ok end

implement main0 () = let
  var abc = @[char][3]('a', 'b', 'c')
  var abd = @[char][3]('a', 'b', 'd')
  var xbc = @[char][3]('x', 'b', 'c')
  var ab = @[char][2]('a', 'b')
  val r1 = check_case("abc=abc", $S.from_char_array(abc, 3), 3, $S.from_char_array(abc, 3), 3, true)
  val r2 = check_case("abc!=abd (last)", $S.from_char_array(abc, 3), 3, $S.from_char_array(abd, 3), 3, false)
  val r3 = check_case("abc!=xbc (first)", $S.from_char_array(abc, 3), 3, $S.from_char_array(xbc, 3), 3, false)
  val r4 = check_case("ab!=abc (length)", $S.from_char_array(ab, 2), 2, $S.from_char_array(abc, 3), 3, false)
  val r5 = check_case("abc!=ab (length)", $S.from_char_array(abc, 3), 3, $S.from_char_array(ab, 2), 2, false)
in
  if r1 && r2 && r3 && r4 && r5 then println! ("eq: all cases pass")
  else exit_void(1)
end
