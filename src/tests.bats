(* tests -- unit tests for str *)
(* These are plain functions; $UNITTEST.run wraps them when the *)
(* self-hosting compiler is used.  bats-old sees them as dead code. *)

#include "share/atspre_staload.hats"

#use array as A
#use arith as AR

staload "./lib.sats"

fn test_from_char_array_hello(): bool = let
  var !p = @[char]('H', 'e', 'l', 'l', 'o')
  val arr = from_char_array(!p, 5)
  val b0 = byte2int0($A.get<byte>(arr, 0))
  val b1 = byte2int0($A.get<byte>(arr, 1))
  val b2 = byte2int0($A.get<byte>(arr, 2))
  val b3 = byte2int0($A.get<byte>(arr, 3))
  val b4 = byte2int0($A.get<byte>(arr, 4))
  val () = $A.free<byte>(arr)
in
  $AR.eq_int_int(b0, 72) && $AR.eq_int_int(b1, 101) &&
  $AR.eq_int_int(b2, 108) && $AR.eq_int_int(b3, 108) &&
  $AR.eq_int_int(b4, 111)
end

fn test_from_char_array_single(): bool = let
  var !p = @[char]('X')
  val arr = from_char_array(!p, 1)
  val b0 = byte2int0($A.get<byte>(arr, 0))
  val () = $A.free<byte>(arr)
in $AR.eq_int_int(b0, 88) end

fn test_from_char_array_digits(): bool = let
  var !p = @[char]('0', '1', '2')
  val arr = from_char_array(!p, 3)
  val b0 = byte2int0($A.get<byte>(arr, 0))
  val b1 = byte2int0($A.get<byte>(arr, 1))
  val b2 = byte2int0($A.get<byte>(arr, 2))
  val () = $A.free<byte>(arr)
in
  $AR.eq_int_int(b0, 48) && $AR.eq_int_int(b1, 49) &&
  $AR.eq_int_int(b2, 50)
end
