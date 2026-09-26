#include "share/atspre_staload.hats"
#use array as A
#use str as S

(* trim_left / trim_right positions. Exits 1 on any mismatch. *)
fn check (name: string, got: int, want: int): bool = let
  val ok = (got = want)
  val () = (if ok then () else println! ("FAIL ", name, ": got ", got, ", want ", want))
in ok end

fn tl {l:agz}{n:pos} (bv: !$A.borrow(byte, l, n), n: int n): int = $S.trim_left(bv, n)
fn tr {l:agz}{n:pos} (bv: !$A.borrow(byte, l, n), n: int n): int = $S.trim_right(bv, n)

implement main0 () = let
  var a = @[char][6](' ', ' ', 'x', 'y', ' ', '\t')
  var b = @[char][3](' ', '\n', ' ')
  var c = @[char][2]('o', 'k')
  val @(fa, ba) = $A.freeze<byte>($S.from_char_array(a, 6))
  val @(fb, bb) = $A.freeze<byte>($S.from_char_array(b, 3))
  val @(fc, bc) = $A.freeze<byte>($S.from_char_array(c, 2))
  val r1 = check("left '  xy \\t'", tl(ba, 6), 2)
  val r2 = check("right '  xy \\t'", tr(ba, 6), 4)
  val r3 = check("left all-space", tl(bb, 3), 3)
  val r4 = check("right all-space", tr(bb, 3), 0)
  val r5 = check("left 'ok'", tl(bc, 2), 0)
  val r6 = check("right 'ok'", tr(bc, 2), 2)
  val () = $A.drop<byte>(fa, ba) val () = $A.free<byte>($A.thaw<byte>(fa))
  val () = $A.drop<byte>(fb, bb) val () = $A.free<byte>($A.thaw<byte>(fb))
  val () = $A.drop<byte>(fc, bc) val () = $A.free<byte>($A.thaw<byte>(fc))
in
  if r1 && r2 && r3 && r4 && r5 && r6 then println! ("trim: all cases pass")
  else exit_void(1)
end
