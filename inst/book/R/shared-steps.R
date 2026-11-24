# Shared helpers: the What to test steps and the proofs that tick them. A step is ticked by the server, from inputs it
# really received, Checklist on or off; the page only draws the ticks. A step without a proof is ticked by hand.

# A CSS selector for the tag whose tip has the id `id` (the id given to tip_on(id =)).
tip_target <- function(id) sprintf("[%s=\"%s\"]", book_contract()$attributes$id, id)

# One step: what to do (with what should happen), what would be a bug, where in the figure it points (a CSS selector
# for Checklist's ring and leader) and its proof, or NULL.
step <- function(text, bug = NULL, target, proof = NULL) {
  list(text = text, bug = bug, target = target, proof = proof)
}

# A proof: the inputs to watch and a test of one input's value. `test(value, input)` returns TRUE when the step is
# done; `input` lets a test read the page's other settings.
proof <- function(inputs, test) {
  list(inputs = inputs, test = test)
}

# A field of an input's value matches when the wanted value is NULL (any) or equal.
fields_match <- function(value, ...) {
  wanted <- list(...)
  all(vapply(
    names(wanted),
    function(f) is.null(wanted[[f]]) || (!is.null(value[[f]]) && value[[f]] %in% wanted[[f]]),
    NA
  ))
}

# A test of the page's settings for a proof's `when`: TRUE while the choice set `name` holds `value`. `default` is
# the set's first choice, which the page holds before the reader picks one.
chose <- function(name, value, default) function(input) identical(input[[name]] %||% default, value)

# Proofs from the tip inputs. `by = "reader"` by default: a tip closed and pinned again by the page (a look change),
# or closed because its tag was drawn again (`by = "data"`), never ticks a step.
on_hint <- function(id, state = "shown", reason = NULL, via = NULL, by = "reader", when = NULL) {
  proof(
    paste0(id, "_hint"),
    function(value, input) {
      fields_match(value, state = state, reason = reason, via = via, by = by) && (is.null(when) || isTRUE(when(input)))
    }
  )
}

on_pin <- function(id, state = "pinned", reason = NULL, by = "reader", when = NULL) {
  proof(
    paste0(id, "_tip"),
    function(value, input) {
      fields_match(value, state = state, reason = reason, by = by) && (is.null(when) || isTRUE(when(input)))
    }
  )
}

on_copy <- function(id) {
  proof(paste0(id, "_copy"), function(value, input) isTRUE(value$ok))
}

# What a proof's input says, for the line under the steps: input$id_tip, state "pinned", reason "escape".
proof_says <- function(name, value) {
  bits <- if (is.list(value)) {
    Filter(
      Negate(is.null),
      list(
        if (!is.null(value$state)) paste0("state \"", value$state, "\""),
        if (!is.null(value$reason)) paste0("reason \"", value$reason, "\""),
        if (!is.null(value$by) && !identical(value$by, "reader")) paste0("by \"", value$by, "\"")
      )
    )
  }
  if (startsWith(name, "book:")) {
    return(as.character(value))
  }
  paste(c(input_ref(name), unlist(bits)), collapse = ", ")
}

# The server side of a plate's steps. Watches every input a proof names, ticks the steps they prove and sends the
# ticked set to the page. A click on a step's box (`input$book_tick`) ticks a step without a proof, and unticks any
# step, so it can be tried again; a step with a proof is ticked only by its proof, so a tick means the server saw it.
# Returns
# `check(name, value)`, so a plate can prove a step from something the server saw itself (an output drawn or kept).
steps_server <- function(input, session, plate, steps) {
  done <- shiny::reactiveVal(integer())
  send <- function(said = NULL) {
    session$sendCustomMessage("book-ticks", list(plate = plate, done = as.list(done()), said = said))
  }
  tick <- function(i, on = TRUE, said = NULL) {
    now <- if (on) union(done(), i) else setdiff(done(), i)
    done(sort(now))
    send(said)
  }
  check <- function(name, value) {
    for (i in seq_along(steps)) {
      p <- steps[[i]]$proof
      if (is.null(p) || !name %in% p$inputs || (i - 1L) %in% shiny::isolate(done())) {
        next
      }
      ok <- tryCatch(isTRUE(p$test(value, input)), error = function(e) FALSE)
      if (!ok) {
        next
      }
      tick(i - 1L, TRUE, paste0("Step ", i, " ticked itself: ", proof_says(name, value), "."))
    }
  }
  names <- as.character(unique(unlist(lapply(steps, function(s) s$proof$inputs))))
  for (name in names[!startsWith(names, "book:")]) {
    local({
      name <- name
      shiny::observeEvent(input[[name]], check(name, input[[name]]), ignoreInit = TRUE)
    })
  }
  shiny::observeEvent(input$book_tick, {
    t <- input$book_tick
    if (!identical(t$plate, plate) || !checkmate::test_int(t$i, lower = 0, upper = length(steps) - 1)) {
      return()
    }
    i <- as.integer(t$i)
    if (isTRUE(t$on) && !is.null(steps[[i + 1L]]$proof)) {
      send(paste0("Step ", i + 1L, " ticks itself when the server receives its input."))
      return()
    }
    tick(i, isTRUE(t$on))
  })
  shiny::observeEvent(input$book_ready, send(), ignoreInit = TRUE)
  list(check = check, done = done)
}
