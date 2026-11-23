# the record errors / name every value that cannot be shown

    Code
      tip_record(list(a = list(1), b = 1:2, c = I("x"), d = 0+1i, ok = 1))
    Condition
      Error:
      ! `record` has values a tip cannot show.
      x `record$a` is a list; a tip shows a dash for it. Give one value.
      x `record$b` has 2 values; give one.
      x `record$c` is wrapped in `I()`; a record holds single values, so drop `I()`.
      x `record$d` is complex; give text, a number or true or false.
      i Keep only the columns the template names.

# the record errors / say what to give instead of a data frame of many rows

    Code
      tip_record(data.frame(a = 1:2))
    Condition
      Error:
      ! `record` is a data frame of 2 rows; a tip takes one.
      i Give one row, such as `data[1, ]`, or a named list.

# the record errors / name a column with braces as it is

    Code
      tip_record(list(`a{b}` = list(1)))
    Condition
      Error:
      ! `record` has values a tip cannot show.
      x `record$a{b}` is a list; a tip shows a dash for it. Give one value.
      i Keep only the columns the template names.

