#include "share/atspre_staload.hats"
#use array as A
#use str as S

(* index_of / contains on "a:b:c". Exits 1 on any mismatch. *)
fn idx {l:agz}{n:pos} (bv: !$A.borrow(byte, l, n), n: int n, c: int): int =
  case+ $S.index_of(bv, n, c) of
  | ~$S.str_some(i) => i
  | ~$S.str_none() => ~1

fn check_int (name: string, got: int, want: int): bool = let
  val ok = (got = want)
  val () = (if ok then () else println! ("FAIL ", name, ": got ", got, ", want ", want))
in ok end

fn check_bool (name: string, got: bool, want: bool): bool = let
  val ok = (got = want)
  val () = (if ok then () else println! ("FAIL ", name, ": got ", got, ", want ", want))
in ok end

implement main0 () = let
  var s = @[char][5]('a', ':', 'b', ':', 'c')
  val @(fs, bs) = $A.freeze<byte>($S.from_char_array(s, 5))
  val r1 = check_int("index_of ':' (first)", idx(bs, 5, 58), 1)
  val r2 = check_int("index_of 'a' (at 0)", idx(bs, 5, 97), 0)
  val r3 = check_int("index_of 'c' (last)", idx(bs, 5, 99), 4)
  val r4 = check_int("index_of 'z' (absent)", idx(bs, 5, 122), ~1)
  val r5 = check_bool("contains 'b'", $S.contains(bs, 5, 98), true)
  val r6 = check_bool("contains 'c' (last)", $S.contains(bs, 5, 99), true)
  val r7 = check_bool("contains 'z'", $S.contains(bs, 5, 122), false)
  val () = $A.drop<byte>(fs, bs)
  val () = $A.free<byte>($A.thaw<byte>(fs))
in
  if r1 && r2 && r3 && r4 && r5 && r6 && r7 then println! ("search: all cases pass")
  else exit_void(1)
end
