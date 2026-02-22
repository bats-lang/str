# str

String operations on byte arrays.

## API

- `compare(a, b)` — lexicographic comparison of two byte strings
- `index_of(haystack, needle)` — find first occurrence of needle in haystack
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

## Dependencies

- array
- arith
