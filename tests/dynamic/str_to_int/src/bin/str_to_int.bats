#include "share/atspre_staload.hats"
#use array as A
#use str as S

(* str_to_int on whole buffers. want_ok = false means none is expected.
   Exits 1 on any mismatch. *)
fn run {k:pos | k <= 1048576} (name: string, src: &(@[char][k]), k: int k, want_ok: bool, want: int): bool = let
  val @(f, b) = $A.freeze<byte>($S.from_char_array(src, k))
  val r = $S.str_to_int(b, k)
  val () = $A.drop<byte>(f, b)
  val () = $A.free<byte>($A.thaw<byte>(f))
  val ok = (case+ r of
    | ~$S.str_some(v) => want_ok && v = want
    | ~$S.str_none() => ~want_ok)
  val () = (if ok then () else println! ("FAIL ", name))
in ok end

implement main0 () = let
  var a = @[char][3]('4', '2', '7')
  var b = @[char][1]('0')
  var c = @[char][3]('-', '1', '5')
  var d = @[char][1]('-')
  var e = @[char][3]('1', 'x', '2')
  var f = @[char][2]('+', '3')
  var g = @[char][2]('0', '9')
  val r1 = run("427", a, 3, true, 427)
  val r2 = run("0", b, 1, true, 0)
  val r3 = run("-15", c, 3, true, ~15)
  val r4 = run("- alone", d, 1, false, 0)
  val r5 = run("1x2", e, 3, false, 0)
  val r6 = run("+3", f, 2, false, 0)
  val r7 = run("09", g, 2, true, 9)
in
  if r1 && r2 && r3 && r4 && r5 && r6 && r7 then println! ("str_to_int: all cases pass")
  else exit_void(1)
end
