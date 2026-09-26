#include "share/atspre_staload.hats"
#use array as A
#use str as S

(* int_to_str into a 12-byte buffer at position 1: the returned end
   position and the bytes written must match. Exits 1 on any mismatch. *)
fn run {k:pos | k + 1 <= 12}{v:int}
  (name: string, value: int v, want: &(@[char][k]), k: int k): bool = let
  val buf = $A.alloc<byte>(12)
  val r = $S.int_to_str(buf, 1, 12, value)
  val @(fw, bw) = $A.freeze<byte>($S.from_char_array(want, k))
  val same = $S.match_at_arr(buf, 1, bw, k)
  val () = $A.drop<byte>(fw, bw)
  val () = $A.free<byte>($A.thaw<byte>(fw))
  val () = $A.free<byte>(buf)
  val ok = (r = 1 + k) && same
  val () = (if ok then () else println! ("FAIL ", name, ": end ", r, ", bytes match ", same))
in ok end

implement main0 () = let
  var c0 = @[char][1]('0')
  var c7 = @[char][1]('7')
  var c10 = @[char][2]('1', '0')
  var c5 = @[char][5]('9', '0', '8', '0', '7')
  var cn5 = @[char][2]('-', '5')
  var cn120 = @[char][4]('-', '1', '2', '0')
  var cmax = @[char][10]('2', '1', '4', '7', '4', '8', '3', '6', '4', '7')
  var cmin = @[char][11]('-', '2', '1', '4', '7', '4', '8', '3', '6', '4', '8')
  var cmin1 = @[char][11]('-', '2', '1', '4', '7', '4', '8', '3', '6', '4', '7')
  var cn9 = @[char][2]('-', '9')
  var cn10 = @[char][3]('-', '1', '0')
  var cn19 = @[char][3]('-', '1', '9')
  val r1 = run("0", 0, c0, 1)
  val r2 = run("7", 7, c7, 1)
  val r3 = run("10", 10, c10, 2)
  val r4 = run("90807", 90807, c5, 5)
  val r5 = run("-5", ~5, cn5, 2)
  val r6 = run("-120", ~120, cn120, 4)
  val r7 = run("2147483647", 2147483647, cmax, 10)
  (* The minimum int has no positive counterpart. *)
  val r9 = run("-2147483648", ~2147483647 - 1, cmin, 11)
  val r10 = run("-2147483647", ~2147483647, cmin1, 11)
  val r11 = run("-9", ~9, cn9, 2)
  val r12 = run("-10", ~10, cn10, 3)
  val r13 = run("-19", ~19, cn19, 3)
  (* Does not fit: 5 digits at position 1 in a 4-byte buffer. *)
  val small = $A.alloc<byte>(4)
  val rs = $S.int_to_str(small, 1, 4, 12345)
  val () = $A.free<byte>(small)
  val r8 = (rs = 1)
  val () = (if r8 then () else println! ("FAIL no-fit: end ", rs, ", want 1"))
in
  if r1 && r2 && r3 && r4 && r5 && r6 && r7 && r8 && r9 && r10 && r11 && r12 && r13 then println! ("int_to_str: all cases pass")
  else exit_void(1)
end
