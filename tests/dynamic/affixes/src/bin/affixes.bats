#include "share/atspre_staload.hats"
#use array as A
#use str as S

(* starts_with / ends_with on "abcd". Exits 1 on any mismatch. *)
fn check (name: string, got: bool, want: bool): bool = let
  val ok = (got = want)
  val () = (if ok then () else println! ("FAIL ", name, ": got ", got, ", want ", want))
in ok end

implement main0 () = let
  var s = @[char][4]('a', 'b', 'c', 'd')
  var ab = @[char][2]('a', 'b')
  var cd = @[char][2]('c', 'd')
  var bc = @[char][2]('b', 'c')
  var long = @[char][5]('a', 'b', 'c', 'd', 'e')
  val @(fs, bs) = $A.freeze<byte>($S.from_char_array(s, 4))
  val @(f1, p_ab) = $A.freeze<byte>($S.from_char_array(ab, 2))
  val @(f2, p_cd) = $A.freeze<byte>($S.from_char_array(cd, 2))
  val @(f3, p_bc) = $A.freeze<byte>($S.from_char_array(bc, 2))
  val @(f4, p_lg) = $A.freeze<byte>($S.from_char_array(long, 5))
  val @(f5, p_s) = $A.freeze<byte>($S.from_char_array(s, 4))
  val r1 = check("starts ab", $S.starts_with(bs, 4, p_ab, 2), true)
  val r2 = check("starts cd", $S.starts_with(bs, 4, p_cd, 2), false)
  val r3 = check("starts self", $S.starts_with(bs, 4, p_s, 4), true)
  val r4 = check("starts longer", $S.starts_with(bs, 4, p_lg, 5), false)
  val r5 = check("ends cd", $S.ends_with(bs, 4, p_cd, 2), true)
  val r6 = check("ends ab", $S.ends_with(bs, 4, p_ab, 2), false)
  val r7 = check("ends bc", $S.ends_with(bs, 4, p_bc, 2), false)
  val r8 = check("ends longer", $S.ends_with(bs, 4, p_lg, 5), false)
  val () = $A.drop<byte>(fs, bs) val () = $A.free<byte>($A.thaw<byte>(fs))
  val () = $A.drop<byte>(f1, p_ab) val () = $A.free<byte>($A.thaw<byte>(f1))
  val () = $A.drop<byte>(f2, p_cd) val () = $A.free<byte>($A.thaw<byte>(f2))
  val () = $A.drop<byte>(f3, p_bc) val () = $A.free<byte>($A.thaw<byte>(f3))
  val () = $A.drop<byte>(f4, p_lg) val () = $A.free<byte>($A.thaw<byte>(f4))
  val () = $A.drop<byte>(f5, p_s) val () = $A.free<byte>($A.thaw<byte>(f5))
in
  if r1 && r2 && r3 && r4 && r5 && r6 && r7 && r8 then println! ("affixes: all cases pass")
  else exit_void(1)
end
