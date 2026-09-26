#include "share/atspre_staload.hats"
#use array as A
#use str as S

(* name_eq on an 8-byte buffer "src\0...." with several logical
   lengths. Exits 1 on any mismatch. *)
fn check (name: string, got: bool, want: bool): bool = let
  val ok = (got = want)
  val () = (if ok then () else println! ("FAIL ", name, ": got ", got, ", want ", want))
in ok end

implement main0 () = let
  var s = @[char][8]('s', 'r', 'c', '\000', 'x', 'x', 'x', 'x')
  var w = @[char][3]('s', 'r', 'c')
  var v = @[char][3]('s', 'r', 'x')
  val buf = $S.from_char_array(s, 8)
  val @(fw, bw) = $A.freeze<byte>($S.from_char_array(w, 3))
  val @(fv, bv) = $A.freeze<byte>($S.from_char_array(v, 3))
  val r1 = check("src = src", $S.name_eq(buf, 3, 8, bw, 3), true)
  val r2 = check("src != srx", $S.name_eq(buf, 3, 8, bv, 3), false)
  val r3 = check("len 2 != 3", $S.name_eq(buf, 2, 8, bw, 3), false)
  val r4 = check("len 4 != 3", $S.name_eq(buf, 4, 8, bw, 3), false)
  val () = $A.free<byte>(buf)
  val () = $A.drop<byte>(fw, bw) val () = $A.free<byte>($A.thaw<byte>(fw))
  val () = $A.drop<byte>(fv, bv) val () = $A.free<byte>($A.thaw<byte>(fv))
in
  if r1 && r2 && r3 && r4 then println! ("name_eq: all cases pass")
  else exit_void(1)
end
