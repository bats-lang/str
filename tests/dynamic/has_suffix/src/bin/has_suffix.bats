#include "share/atspre_staload.hats"
#use array as A
#use str as S

(* has_suffix on an 8-byte buffer "a.dats\0\0" with logical lengths
   0..8. Exits 1 on any mismatch. *)
fn check (name: string, got: bool, want: bool): bool = let
  val ok = (got = want)
  val () = (if ok then () else println! ("FAIL ", name, ": got ", got, ", want ", want))
in ok end

implement main0 () = let
  var s = @[char][8]('a', '.', 'd', 'a', 't', 's', '\000', '\000')
  var ext = @[char][5]('.', 'd', 'a', 't', 's')
  val buf = $S.from_char_array(s, 8)
  val @(fe, be) = $A.freeze<byte>($S.from_char_array(ext, 5))
  val r1 = check("len 6: a.dats", $S.has_suffix(buf, 6, 8, be, 5), true)
  val r2 = check("len 5: a.dat", $S.has_suffix(buf, 5, 8, be, 5), false)
  val r3 = check("len 4: shorter than suffix", $S.has_suffix(buf, 4, 8, be, 5), false)
  val r4 = check("len 8: trailing NULs", $S.has_suffix(buf, 8, 8, be, 5), false)
  val r5 = check("len 0", $S.has_suffix(buf, 0, 8, be, 5), false)
  val () = $A.free<byte>(buf)
  val () = $A.drop<byte>(fe, be)
  val () = $A.free<byte>($A.thaw<byte>(fe))
in
  if r1 && r2 && r3 && r4 && r5 then println! ("has_suffix: all cases pass")
  else exit_void(1)
end
