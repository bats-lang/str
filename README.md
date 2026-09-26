# str

String operations on byte arrays.

## API

- `compare(a, b)` — lexicographic comparison of two byte strings; the result is typed `[r | -1 <= r <= 1]`
- `index_of(haystack, needle)` — find first occurrence of needle in haystack; the index returned is typed `< n`
- `starts_with(s, prefix)` — test whether s begins with prefix
- `ends_with(s, suffix)` — test whether s ends with suffix
- `split(s, delim)` — split s into an array of substrings by delimiter
- `trim(s)` — remove leading and trailing whitespace
- `to_upper(s)` — convert ASCII lowercase to uppercase
- `to_lower(s)` — convert ASCII uppercase to lowercase
- `contains(haystack, needle)` — test whether haystack contains needle
- `copy_to(src, dst)` — copy bytes from src into dst
- `int_to_str(n)` — convert an integer to its decimal string representation
- `str_to_int(s)` — parse a decimal string as an integer
- `match_at(src, p, pat, np)` / `match_at_arr(...)` — does `pat` occur at `src[p..]`; `p + np <= n` is required by the type (replaces `chars_match_borrow` / `chars_match`)
- `has_suffix(ent, len, max, suf, slen)` — does `ent[0..len)` end with `suf`; `len <= n` is required by the type
- `name_eq(ent, len, max, s, slen)` — is `ent[0..len)` exactly `s`; `len <= n` is required by the type
- `fill_exact(arr, src, n, slen, i)` — copy `src[i..]` into `arr[i..]`, up to the shorter end
- `byte_at(bv, p)` — byte at a proven index `p < n` (replaces `borrow_byte`, which checks the range at runtime)
- `find_null_at(buf, p, n)` / `find_null_bv_at(bv, p, n)` — index of the first NUL at or after a proven start `p <= n`, or `n`; the result is typed `[r | p <= r <= n]`

## Tests

`tests/static/run.sh <repository>` runs the static tests: packages in
`tests/static/accept/` must type-check against this checkout, and packages
in `tests/static/reject/` must be rejected with the message in their
`expect` file. `tests/dynamic/run.sh <repository>` builds and runs each
binary under `tests/dynamic/`, for behaviour types cannot state (such as
which value `compare` returns); each must exit 0. CI runs both.

## Dependencies

- array
- arith
