#include "share/atspre_staload.hats"
#use array as A
#use str as S

(* compare(a, b) must return exactly -1, 0 or 1 by byte-lexicographic
   order, with a proper prefix ordered first. Exits 1 on any mismatch. *)
fn check_case {na,nb:pos}{la,lb:agz}
  (name: string, a: $A.arr(byte, la, na), a_n: int na,
   b: $A.arr(byte, lb, nb), b_n: int nb, want: int): bool = let
  val @(fa, ba) = $A.freeze<byte>(a)
  val @(fb, bb) = $A.freeze<byte>(b)
  val got = $S.compare(ba, a_n, bb, b_n)
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
  var ab = @[char][2]('a', 'b')
  var a1 = @[char][1]('a')
  var b1 = @[char][1]('b')
  var hi = @[char][1]('\377')
  val r1 = check_case("abc<abd", $S.from_char_array(abc, 3), 3, $S.from_char_array(abd, 3), 3, ~1)
  val r2 = check_case("abd>abc", $S.from_char_array(abd, 3), 3, $S.from_char_array(abc, 3), 3, 1)
  val r3 = check_case("abc=abc", $S.from_char_array(abc, 3), 3, $S.from_char_array(abc, 3), 3, 0)
  val r4 = check_case("ab<abc", $S.from_char_array(ab, 2), 2, $S.from_char_array(abc, 3), 3, ~1)
  val r5 = check_case("abc>ab", $S.from_char_array(abc, 3), 3, $S.from_char_array(ab, 2), 2, 1)
  val r6 = check_case("a<b", $S.from_char_array(a1, 1), 1, $S.from_char_array(b1, 1), 1, ~1)
  val r7 = check_case("a<\377 (unsigned)", $S.from_char_array(a1, 1), 1, $S.from_char_array(hi, 1), 1, ~1)
in
  if r1 && r2 && r3 && r4 && r5 && r6 && r7 then println! ("compare: all cases pass")
  else exit_void(1)
end
