#include "share/atspre_staload.hats"
#use array as A
#use str as S

(* match_at / match_at_arr on "abcdef" with 2-byte patterns at
   positions 0..4 (the last is exactly at the end). Exits 1 on any
   mismatch. *)
fn check (name: string, got: bool, want: bool): bool = let
  val ok = (got = want)
  val () = (if ok then () else println! ("FAIL ", name, ": got ", got, ", want ", want))
in ok end

implement main0 () = let
  var s = @[char][6]('a', 'b', 'c', 'd', 'e', 'f')
  var ab = @[char][2]('a', 'b')
  var cd = @[char][2]('c', 'd')
  var ef = @[char][2]('e', 'f')
  var ax = @[char][2]('a', 'x')
  val src = $S.from_char_array(s, 6)
  val @(fs, bs) = $A.freeze<byte>($S.from_char_array(s, 6))
  val @(f1, p_ab) = $A.freeze<byte>($S.from_char_array(ab, 2))
  val @(f2, p_cd) = $A.freeze<byte>($S.from_char_array(cd, 2))
  val @(f3, p_ef) = $A.freeze<byte>($S.from_char_array(ef, 2))
  val @(f4, p_ax) = $A.freeze<byte>($S.from_char_array(ax, 2))
  val r1 = check("ab@0", $S.match_at(bs, 0, p_ab, 2), true)
  val r2 = check("cd@2", $S.match_at(bs, 2, p_cd, 2), true)
  val r3 = check("ef@4 (end)", $S.match_at(bs, 4, p_ef, 2), true)
  val r4 = check("ab@1", $S.match_at(bs, 1, p_ab, 2), false)
  val r5 = check("ax@0 (second byte)", $S.match_at(bs, 0, p_ax, 2), false)
  val r6 = check("arr cd@2", $S.match_at_arr(src, 2, p_cd, 2), true)
  val r7 = check("arr ef@3", $S.match_at_arr(src, 3, p_ef, 2), false)
  val () = $A.free<byte>(src)
  val () = $A.drop<byte>(fs, bs) val () = $A.free<byte>($A.thaw<byte>(fs))
  val () = $A.drop<byte>(f1, p_ab) val () = $A.free<byte>($A.thaw<byte>(f1))
  val () = $A.drop<byte>(f2, p_cd) val () = $A.free<byte>($A.thaw<byte>(f2))
  val () = $A.drop<byte>(f3, p_ef) val () = $A.free<byte>($A.thaw<byte>(f3))
  val () = $A.drop<byte>(f4, p_ax) val () = $A.free<byte>($A.thaw<byte>(f4))
in
  if r1 && r2 && r3 && r4 && r5 && r6 && r7 then println! ("match_at: all cases pass")
  else exit_void(1)
end
