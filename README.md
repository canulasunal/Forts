# Fortran Strings

Forts is a lightweight single file string manipulation for Fortran 90+ providing basic string manipulation capabilities to the language. This library is completely open source and free. Below are the currently supported commands:

replace(original_string, substring, new_substring) ! returns string
startswith(string, substring) ! returns 0 or 1 (false or true)
endswith(string, substring) ! returns 0 or 1 (false or true)
copy(string, times) ! returns string
