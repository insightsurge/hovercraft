# the placeholder errors / name every mistake in a text

    Code
      tip_check_text(
        "{{ a | numbr:2 }} {{ b | date:d }} {{ }} {{ c | number:two }} {{ d {{ e }} {{ f",
        "The text")
    Condition
      Error:
      ! The text has a placeholder the engine cannot use.
      x {{ a | numbr:2 }}: no filter "numbr"; the filters are number, percent
      x {{ b | date:d }}: the filter "date" comes with a later version
      x {{ }} names no field; write one, such as {{label}}
      x {{ c | number:two }}: "number" takes a whole number from 0 to 100 after the colon (no quotes)
      x {{ d {{ e }} holds "{{" inside a placeholder; close the first one before opening another
      x "{{" at character 76 never closes; add "}}", or write the words without it
      i A placeholder is `{{ field }}` or `{{ field | number:2 }}`, with one filter at most.

# the placeholder errors / say a chain of filters is one too many

    Code
      tip_check_text("{{ a | number:1 | percent:1 }}", "The text")
    Condition
      Error:
      ! The text has a placeholder the engine cannot use.
      x {{ a | number:1 | percent:1 }}: one filter only; a placeholder takes no chain of filters
      i A placeholder is `{{ field }}` or `{{ field | number:2 }}`, with one filter at most.

