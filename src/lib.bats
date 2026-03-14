(* str -- byte array operations *)
(* Pure computation on borrowed byte arrays. No $UNSAFE, no assume. *)

#include "share/atspre_staload.hats"

#use array as A
#use arith as AR

(* ============================================================
   Option type for returning optional int values
   ============================================================ *)

#pub datavtype str_option(a:t@ype) =
  | str_some(a) of (a)
  | str_none(a) of ()

(* ============================================================
   compare -- lexicographic comparison, returns -1/0/1
   ============================================================ *)

#pub fun compare
  {la:agz}{na:pos}{lb:agz}{nb:pos}
  (a: !$A.borrow(byte, la, na), a_len: int na,
   b: !$A.borrow(byte, lb, nb), b_len: int nb): int

(* ============================================================
   eq -- equality check
   ============================================================ *)

#pub fun eq
  {la:agz}{na:pos}{lb:agz}{nb:pos}
  (a: !$A.borrow(byte, la, na), a_len: int na,
   b: !$A.borrow(byte, lb, nb), b_len: int nb): bool

(* ============================================================
   index_of -- first occurrence of a byte
   ============================================================ *)

#pub fun index_of
  {la:agz}{na:pos}
  (haystack: !$A.borrow(byte, la, na), h_len: int na,
   needle_byte: int): str_option(int)

(* ============================================================
   starts_with -- test whether s begins with pfx
   ============================================================ *)

#pub fun starts_with
  {la:agz}{na:pos}{lb:agz}{nb:pos}
  (s: !$A.borrow(byte, la, na), s_len: int na,
   pfx: !$A.borrow(byte, lb, nb), p_len: int nb): bool

(* ============================================================
   ends_with -- test whether s ends with suffix
   ============================================================ *)

#pub fun ends_with
  {la:agz}{na:pos}{lb:agz}{nb:pos}
  (s: !$A.borrow(byte, la, na), s_len: int na,
   suffix: !$A.borrow(byte, lb, nb), sf_len: int nb): bool

(* ============================================================
   contains -- test whether s contains a given byte value
   ============================================================ *)

#pub fun contains
  {la:agz}{na:pos}
  (s: !$A.borrow(byte, la, na), s_len: int na,
   byte_val: int): bool

(* ============================================================
   trim_left -- returns new start offset (skips spaces/tabs/newlines)
   ============================================================ *)

#pub fun trim_left
  {la:agz}{na:pos}
  (s: !$A.borrow(byte, la, na), s_len: int na): int

(* ============================================================
   trim_right -- returns new end position
   ============================================================ *)

#pub fun trim_right
  {la:agz}{na:pos}
  (s: !$A.borrow(byte, la, na), s_len: int na): int

(* ============================================================
   to_upper_byte -- single byte a-z -> A-Z
   ============================================================ *)

#pub fun to_upper_byte(b: int): int

(* ============================================================
   to_lower_byte -- single byte A-Z -> a-z
   ============================================================ *)

#pub fun to_lower_byte(b: int): int

(* ============================================================
   int_to_str -- write decimal representation into buffer, return new pos
   ============================================================ *)

#pub fun int_to_str
  {l:agz}{n:pos}
  (buf: !$A.arr(byte, l, n), pos: int, max_len: int n, value: int): int

(* ============================================================
   str_to_int -- parse decimal integer from byte array
   ============================================================ *)

#pub fun str_to_int
  {lb:agz}{n:pos}
  (s: !$A.borrow(byte, lb, n), len: int n): str_option(int)

(* ============================================================
   from_char_array -- create arr(byte) from a flat char array literal
   ============================================================ *)

#pub fn from_char_array
  {n:pos | n <= 1048576}
  (src: &(@[char][n]), n: int n): [l:agz] $A.arr(byte, l, n)

(* ============================================================
   text_of_chars -- create text(n) from a flat char array literal
   ============================================================ *)

#pub fn text_of_chars
  {n:pos | n <= 1048576}
  (src: &(@[char][n]), n: int n): $A.text(n)

(* ============================================================
   chars_match -- check bytes in arr at offset against a borrow
   ============================================================ *)

#pub fun chars_match
  {l:agz}{n:pos}{lp:agz}{np:pos}
  (ent: !$A.arr(byte, l, n), p: int, max: int n,
   pat: !$A.borrow(byte, lp, np), pi: int, plen: int np): bool

(* ============================================================
   chars_match_borrow -- like chars_match but for borrow arrays
   ============================================================ *)

#pub fun chars_match_borrow
  {l:agz}{n:pos}{lp:agz}{np:pos}
  (src: !$A.borrow(byte, l, n), p: int, max: int n,
   pat: !$A.borrow(byte, lp, np), pi: int, plen: int np): bool

(* ============================================================
   has_suffix -- check if name ends with a borrow suffix
   ============================================================ *)

#pub fn has_suffix
  {l:agz}{n:pos}{lp:agz}{np:pos}
  (ent: !$A.arr(byte, l, n), len: int, max: int n,
   suf: !$A.borrow(byte, lp, np), slen: int np): bool

(* ============================================================
   name_eq -- check if name exactly matches a borrow
   ============================================================ *)

#pub fn name_eq
  {l:agz}{n:pos}{lp:agz}{np:pos}
  (ent: !$A.arr(byte, l, n), len: int, max: int n,
   s: !$A.borrow(byte, lp, np), slen: int np): bool

(* ============================================================
   Whitespace helper
   ============================================================ *)

fn _is_whitespace(c: int): bool =
  if $AR.eq_int_int(c, 32) then true
  else if $AR.eq_int_int(c, 9) then true
  else if $AR.eq_int_int(c, 10) then true
  else if $AR.eq_int_int(c, 13) then true
  else false

(* ============================================================
   Implementations
   ============================================================ *)

(* -- compare -- *)

implement compare (a, a_len, b, b_len) = let
  val min_len = (if $AR.lt_int_int(a_len, b_len) then a_len else b_len): int
  fun loop {la:agz}{na:pos}{lb:agz}{nb:pos}{k:nat} .<k>.
    (a: !$A.borrow(byte, la, na), a_len: int na,
     b: !$A.borrow(byte, lb, nb), b_len: int nb,
     i: int, min_l: int, rem: int(k)): int =
    if rem <= 0 then 0
    else if $AR.gte_int_int(i, min_l) then
      if $AR.lt_int_int(a_len, b_len) then ~1
      else if $AR.gt_int_int(a_len, b_len) then 1
      else 0
    else let
      val ca = byte2int0($A.read<byte>(a, $AR.checked_idx(i, a_len)))
      val cb = byte2int0($A.read<byte>(b, $AR.checked_idx(i, b_len)))
    in
      if $AR.lt_int_int(ca, cb) then ~1
      else if $AR.gt_int_int(ca, cb) then 1
      else loop(a, a_len, b, b_len, i + 1, min_l, rem - 1)
    end
  val fuel = $AR.checked_nat(min_len + 1)
in loop(a, a_len, b, b_len, 0, min_len, fuel) end

(* -- eq -- *)

implement eq (a, a_len, b, b_len) = let
  fun loop {la:agz}{na:pos}{lb:agz}{nb:pos}{k:nat} .<k>.
    (a: !$A.borrow(byte, la, na), a_len: int na,
     b: !$A.borrow(byte, lb, nb), b_len: int nb,
     i: int, rem: int(k)): bool =
    if rem <= 0 then true
    else if $AR.gte_int_int(i, a_len) then true
    else let
      val ca = byte2int0($A.read<byte>(a, $AR.checked_idx(i, a_len)))
      val cb = byte2int0($A.read<byte>(b, $AR.checked_idx(i, b_len)))
    in
      if $AR.neq_int_int(ca, cb) then false
      else loop(a, a_len, b, b_len, i + 1, rem - 1)
    end
in
  if $AR.neq_int_int(a_len, b_len) then false
  else loop(a, a_len, b, b_len, 0, $AR.checked_nat(a_len))
end

(* -- index_of -- *)

implement index_of {la}{na} (haystack, h_len, needle_byte) = let
  fun loop {la:agz}{na:pos}{k:nat} .<k>.
    (h: !$A.borrow(byte, la, na), h_len: int na,
     needle: int, i: int, rem: int(k)): str_option(int) =
    if rem <= 0 then str_none()
    else if $AR.gte_int_int(i, h_len) then str_none()
    else let
      val c = byte2int0($A.read<byte>(h, $AR.checked_idx(i, h_len)))
    in
      if $AR.eq_int_int(c, needle) then str_some(i)
      else loop(h, h_len, needle, i + 1, rem - 1)
    end
in loop(haystack, h_len, needle_byte, 0, $AR.checked_nat(h_len)) end

(* -- starts_with -- *)

implement starts_with (s, s_len, pfx, p_len) = let
  fun loop {la:agz}{na:pos}{lb:agz}{nb:pos}{k:nat} .<k>.
    (s: !$A.borrow(byte, la, na), s_len: int na,
     pfx: !$A.borrow(byte, lb, nb), p_len: int nb,
     i: int, rem: int(k)): bool =
    if rem <= 0 then true
    else if $AR.gte_int_int(i, p_len) then true
    else let
      val cs = byte2int0($A.read<byte>(s, $AR.checked_idx(i, s_len)))
      val cp = byte2int0($A.read<byte>(pfx, $AR.checked_idx(i, p_len)))
    in
      if $AR.neq_int_int(cs, cp) then false
      else loop(s, s_len, pfx, p_len, i + 1, rem - 1)
    end
in
  if $AR.gt_int_int(p_len, s_len) then false
  else loop(s, s_len, pfx, p_len, 0, $AR.checked_nat(p_len))
end

(* -- ends_with -- *)

implement ends_with (s, s_len, suffix, sf_len) = let
  val offset = $AR.sub_int_int(s_len, sf_len)
  fun loop {la:agz}{na:pos}{lb:agz}{nb:pos}{k:nat} .<k>.
    (s: !$A.borrow(byte, la, na), s_len: int na,
     suffix: !$A.borrow(byte, lb, nb), sf_len: int nb,
     i: int, off: int, rem: int(k)): bool =
    if rem <= 0 then true
    else if $AR.gte_int_int(i, sf_len) then true
    else let
      val cs = byte2int0($A.read<byte>(s, $AR.checked_idx(off + i, s_len)))
      val cf = byte2int0($A.read<byte>(suffix, $AR.checked_idx(i, sf_len)))
    in
      if $AR.neq_int_int(cs, cf) then false
      else loop(s, s_len, suffix, sf_len, i + 1, off, rem - 1)
    end
in
  if $AR.gt_int_int(sf_len, s_len) then false
  else loop(s, s_len, suffix, sf_len, 0, offset, $AR.checked_nat(sf_len))
end

(* -- contains -- *)

implement contains {la}{na} (s, s_len, byte_val) = let
  fun loop {la:agz}{na:pos}{k:nat} .<k>.
    (s: !$A.borrow(byte, la, na), s_len: int na,
     bv: int, i: int, rem: int(k)): bool =
    if rem <= 0 then false
    else if $AR.gte_int_int(i, s_len) then false
    else let
      val c = byte2int0($A.read<byte>(s, $AR.checked_idx(i, s_len)))
    in
      if $AR.eq_int_int(c, bv) then true
      else loop(s, s_len, bv, i + 1, rem - 1)
    end
in loop(s, s_len, byte_val, 0, $AR.checked_nat(s_len)) end

(* -- trim_left -- *)

implement trim_left {la}{na} (s, s_len) = let
  fun loop {la:agz}{na:pos}{k:nat} .<k>.
    (s: !$A.borrow(byte, la, na), s_len: int na,
     i: int, rem: int(k)): int =
    if rem <= 0 then i
    else if $AR.gte_int_int(i, s_len) then i
    else let
      val c = byte2int0($A.read<byte>(s, $AR.checked_idx(i, s_len)))
    in
      if _is_whitespace(c) then loop(s, s_len, i + 1, rem - 1)
      else i
    end
in loop(s, s_len, 0, $AR.checked_nat(s_len)) end

(* -- trim_right -- *)

implement trim_right {la}{na} (s, s_len) = let
  fun loop {la:agz}{na:pos}{k:nat} .<k>.
    (s: !$A.borrow(byte, la, na), s_len: int na,
     pos: int, rem: int(k)): int =
    if rem <= 0 then pos
    else if $AR.lte_int_int(pos, 0) then 0
    else let
      val c = byte2int0($A.read<byte>(s, $AR.checked_idx(pos - 1, s_len)))
    in
      if _is_whitespace(c) then loop(s, s_len, pos - 1, rem - 1)
      else pos
    end
in loop(s, s_len, s_len, $AR.checked_nat(s_len)) end

(* -- to_upper_byte -- *)

implement to_upper_byte(b) =
  if $AR.gte_int_int(b, 97) then
    if $AR.lte_int_int(b, 122) then $AR.sub_int_int(b, 32)
    else b
  else b

(* -- to_lower_byte -- *)

implement to_lower_byte(b) =
  if $AR.gte_int_int(b, 65) then
    if $AR.lte_int_int(b, 90) then $AR.add_int_int(b, 32)
    else b
  else b

(* -- int_to_str -- *)

implement int_to_str {l}{n} (buf, pos, max_len, value) = let
  (* Handle negative: write '-' and recurse with positive value *)
  val is_neg = $AR.lt_int_int(value, 0)
  val abs_val = (if is_neg then $AR.sub_int_int(0, value) else value): int

  (* Count digits *)
  fun count_digits {k:nat} .<k>.
    (v: int, rem: int(k)): int =
    if rem <= 0 then 1
    else if $AR.lt_int_int(v, 10) then 1
    else $AR.add_int_int(1, count_digits($AR.div_int_int(v, 10), rem - 1))

  val ndigits = count_digits(abs_val, $AR.checked_nat($AR.add_int_int(abs_val, 1)))
  val total_len = (if is_neg then $AR.add_int_int(ndigits, 1) else ndigits): int

  (* Write digits from right to left *)
  fun write_digits {l:agz}{n:pos}{k:nat} .<k>.
    (buf: !$A.arr(byte, l, n), max_len: int n,
     v: int, wpos: int, rem: int(k)): void =
    if rem <= 0 then ()
    else if $AR.lt_int_int(wpos, 0) then ()
    else if $AR.gte_int_int(wpos, max_len) then ()
    else let
      val digit = $AR.mod_int_int(v, 10)
      val ch = $AR.add_int_int(digit, 48)
      val () = $A.set<byte>(buf, $AR.checked_idx(wpos, max_len),
        $A.int2byte($AR.checked_byte(ch)))
      val next_v = $AR.div_int_int(v, 10)
    in
      if $AR.gt_int_int(next_v, 0) then
        write_digits(buf, max_len, next_v, wpos - 1, rem - 1)
      else ()
    end

  val write_start = pos + total_len - 1
in
  if $AR.gt_int_int(pos + total_len, max_len) then pos
  else let
    val () =
      if is_neg then
        (if $AR.gte_int_int(pos, 0) then
          if $AR.lt_int_int(pos, max_len) then
            $A.set<byte>(buf, $AR.checked_idx(pos, max_len),
              $A.int2byte($AR.checked_byte(45)))
        )
    val () = write_digits(buf, max_len, abs_val, write_start, $AR.checked_nat(total_len))
  in pos + total_len end
end

(* -- str_to_int -- *)

implement str_to_int {lb}{n} (s, len) = let
  val first = byte2int0($A.read<byte>(s, 0))
  val is_neg = $AR.eq_int_int(first, 45)
  val start = (if is_neg then 1 else 0): int

  fun loop {lb:agz}{n:pos}{k:nat} .<k>.
    (s: !$A.borrow(byte, lb, n), slen: int n,
     i: int, acc: int, rem: int(k)): str_option(int) =
    if rem <= 0 then
      (if $AR.gt_int_int(i, start) then
        (if is_neg then str_some($AR.sub_int_int(0, acc))
         else str_some(acc))
       else str_none())
    else if $AR.gte_int_int(i, slen) then
      (if $AR.gt_int_int(i, start) then
        (if is_neg then str_some($AR.sub_int_int(0, acc))
         else str_some(acc))
       else str_none())
    else let
      val c = byte2int0($A.read<byte>(s, $AR.checked_idx(i, slen)))
    in
      if $AR.gte_int_int(c, 48) then
        if $AR.lte_int_int(c, 57) then let
          val digit = $AR.sub_int_int(c, 48)
          val new_acc = $AR.add_int_int($AR.mul_int_int(acc, 10), digit)
        in loop(s, slen, i + 1, new_acc, rem - 1) end
        else str_none()
      else str_none()
    end

  (* Handle single-char "-" *)
in
  if is_neg then
    (if $AR.lte_int_int(len, 1) then str_none()
     else loop(s, len, start, 0, $AR.checked_nat(len)))
  else loop(s, len, start, 0, $AR.checked_nat(len))
end

(* -- from_char_array -- *)

implement from_char_array {n} (src, n) = let
  val arr = $A.alloc<byte>(n)
  fun copy_loop {l:agz}{n:pos}{k:nat | k <= n} .<n - k>.
    (arr: !$A.arr(byte, l, n), src: &(@[char][n]),
     i: int k, n: int n): void =
    if i >= n then ()
    else let
      val () = $A.set<byte>(arr, i, $A.int2byte(
        $AR.byte_of_char(src.[i])))
    in copy_loop(arr, src, i + 1, n) end
  val () = copy_loop(arr, src, 0, n)
in arr end

(* -- text_of_chars -- *)

fn _putc
  {n:pos}{i:nat | i < n}{v:nat | v < 256}
  (b: $A.text_builder(n, i), i: int i, c: int v)
  : $A.text_builder(n, i + 1) =
  $A.text_putc(b, i, c)

fun _text_from_chars {n:pos}{k:nat | k <= n} .<n-k>.
  (b: $A.text_builder(n, k), src: &(@[char][n]),
   i: int k, n: int n): $A.text_builder(n, n) =
  if i >= n then b
  else let
    val cb = $AR.byte_of_char(src.[i])
  in _text_from_chars(_putc(b, i, cb), src, i + 1, n) end

implement text_of_chars {n} (src, n) =
  $A.text_done(_text_from_chars($A.text_build(n), src, 0, n))

(* -- chars_match -- *)

implement chars_match {l}{n}{lp}{np}
  (ent, p, max, pat, pi, plen) = let
  fun loop {l2:agz}{n2:pos}{lp2:agz}{np2:pos}{fuel:nat} .<fuel>.
    (ent: !$A.arr(byte, l2, n2), p: int, max: int n2,
     pat: !$A.borrow(byte, lp2, np2), pi: int, plen: int np2,
     fuel: int fuel): bool =
    if fuel <= 0 then pi >= plen
    else if pi >= plen then true
    else let
      val eb = byte2int0($A.get<byte>(ent, $AR.checked_idx(p + pi, max)))
      val pb = byte2int0($A.read<byte>(pat, $AR.checked_idx(pi, plen)))
    in
      if $AR.eq_int_int(eb, pb) then
        loop(ent, p, max, pat, pi + 1, plen, fuel - 1)
      else false
    end
in loop(ent, p, max, pat, pi, plen, $AR.checked_nat(plen + 1)) end

(* -- chars_match_borrow -- *)

implement chars_match_borrow {l}{n}{lp}{np}
  (src, p, max, pat, pi, plen) = let
  fun loop {l2:agz}{n2:pos}{lp2:agz}{np2:pos}{fuel:nat} .<fuel>.
    (src: !$A.borrow(byte, l2, n2), p: int, max: int n2,
     pat: !$A.borrow(byte, lp2, np2), pi: int, plen: int np2,
     fuel: int fuel): bool =
    if fuel <= 0 then pi >= plen
    else if pi >= plen then true
    else let
      val eb = borrow_byte(src, p + pi, max)
      val pb = byte2int0($A.read<byte>(pat, $AR.checked_idx(pi, plen)))
    in
      if $AR.eq_int_int(eb, pb) then
        loop(src, p, max, pat, pi + 1, plen, fuel - 1)
      else false
    end
in loop(src, p, max, pat, pi, plen, $AR.checked_nat(plen + 1)) end

(* -- has_suffix -- *)

implement has_suffix {l}{n}{lp}{np}
  (ent, len, max, suf, slen) =
  if len < slen then false
  else let val p = $AR.checked_nat(len - slen) in
    chars_match(ent, p, max, suf, 0, slen)
  end

(* -- name_eq -- *)

implement name_eq {l}{n}{lp}{np}
  (ent, len, max, s, slen) =
  if len <> slen then false
  else chars_match(ent, 0, max, s, 0, slen)

(* ============================================================
   Byte reading and null scanning
   ============================================================ *)

(* Read a byte from a borrow, returning 0 for out-of-bounds *)
#pub fn borrow_byte {l:agz}{n:pos}
  (src: !$A.borrow(byte, l, n), pos: int, max: int n): int

implement borrow_byte(src, pos, max) =
  if pos < 0 then 0
  else if pos >= max then 0
  else byte2int0($A.read<byte>(src, $AR.checked_idx(pos, max)))

(* Find null byte in array, starting at pos *)
#pub fun find_null {l:agz}{n:pos}{fuel:nat}
  (buf: !$A.arr(byte, l, n), pos: int, max: int n,
   fuel: int fuel): int

implement find_null(buf, pos, max, fuel) =
  if fuel <= 0 then pos
  else if pos < 0 then pos
  else if pos >= max then pos
  else
    if $AR.eq_int_int(byte2int0($A.get<byte>(buf, $AR.checked_idx(pos, max))), 0) then pos
    else find_null(buf, pos + 1, max, fuel - 1)

(* Find null byte in borrow, starting at pos *)
#pub fun find_null_bv {l:agz}{n:pos}{fuel:nat}
  (bv: !$A.borrow(byte, l, n), pos: int, max: int n,
   fuel: int fuel): int

implement find_null_bv(bv, pos, max, fuel) =
  if fuel <= 0 then pos
  else let
    val b = borrow_byte(bv, pos, max)
  in
    if $AR.eq_int_int(b, 0) then pos
    else find_null_bv(bv, pos + 1, max, fuel - 1)
  end

(* ============================================================
   String to array conversion
   ============================================================ *)

(* Fill array from borrow *)
#pub fun fill_exact {l:agz}{n:pos}{lb:agz}{nb:pos}{i:nat | i <= nb}{fuel:nat}
  (arr: !$A.arr(byte, l, n), src: !$A.borrow(byte, lb, nb), n: int n,
   slen: int nb, i: int i, fuel: int fuel): void

implement fill_exact(arr, src, n, slen, i, fuel) =
  if fuel <= 0 then ()
  else if i >= slen then ()
  else if i >= n then ()
  else let
    val b = $A.read<byte>(src, $AR.checked_idx(i, slen))
    val () = $A.set<byte>(arr, $AR.checked_idx(i, n), b)
  in fill_exact(arr, src, n, slen, i + 1, fuel - 1) end

