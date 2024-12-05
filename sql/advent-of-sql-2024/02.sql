select string_agg(chr(value), '' ORDER BY id)
from letters_b
where (chr(value) >= 'a' and chr(value) <= 'z') or
   (chr(value) >= 'A' and chr(value) <= 'Z')
   or chr(value) in ('!', '''', '\"', '(', ')', ',', '-', '.', ':', ';', '?', ' ');