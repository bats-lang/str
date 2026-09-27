#use array as A
#use str as S

(* The end position is proven in (pos, pos + 11], so a terminator can be
   written after it with no check. *)
#pub fn write_num {l:agz}{v:int} (buf: !$A.arr(byte, l, 16), v: int v): void

implement write_num (buf, v) = let
  val r = $S.int_to_str(buf, 0, 16, v)
in $A.set<byte>(buf, r, $A.int2byte(0)) end
