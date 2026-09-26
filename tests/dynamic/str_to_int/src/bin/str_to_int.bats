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
  var h = @[char][10]('2', '1', '4', '7', '4', '8', '3', '6', '4', '7')
  var i = @[char][11]('-', '2', '1', '4', '7', '4', '8', '3', '6', '4', '8')
  var j = @[char][10]('2', '1', '4', '7', '4', '8', '3', '6', '4', '8')
  var m = @[char][11]('-', '2', '1', '4', '7', '4', '8', '3', '6', '4', '9')
  var o = @[char][11]('9', '9', '9', '9', '9', '9', '9', '9', '9', '9', '9')
  var q = @[char][20]('0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '1')
  val r1 = run("427", a, 3, true, 427)
  val r2 = run("0", b, 1, true, 0)
  val r3 = run("-15", c, 3, true, ~15)
  val r4 = run("- alone", d, 1, false, 0)
  val r5 = run("1x2", e, 3, false, 0)
  val r6 = run("+3", f, 2, false, 0)
  val r7 = run("09", g, 2, true, 9)
  (* Int range: the ends parse, one past them does not. *)
  val r8 = run("max", h, 10, true, 2147483647)
  val r9 = run("min", i, 11, true, ~2147483647 - 1)
  val r10 = run("max + 1", j, 10, false, 0)
  val r11 = run("min - 1", m, 11, false, 0)
  val r12 = run("11 nines", o, 11, false, 0)
  val r13 = run("leading zeros", q, 20, true, 1)
in
  if r1 && r2 && r3 && r4 && r5 && r6 && r7 && r8 && r9 && r10 && r11 && r12 && r13 then println! ("str_to_int: all cases pass")
  else exit_void(1)
end
