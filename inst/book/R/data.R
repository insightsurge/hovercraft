# The book's made-up data: a support desk and its visits table. Nothing here is real.

# The columns of the visits table, with what a tip says about each (section "pins").
book_columns <- data.frame(
  name = c("visit_date", "region", "wait_min"),
  type = c("date", "factor, 3 levels", "double"),
  missing = c("0", "0", "214 (1.7%)"),
  range = c("2026-09-21 to 2026-11-15", "North, Coast, Hills", "0 to 2,880"),
  about = c(
    "The day the visit started, in local time.",
    "The region of the branch that took the visit.",
    "Minutes to the first reply, business hours only."
  ),
  snippet = c(
    "visits |>\n  dplyr::count(week = lubridate::isoweek(visit_date))",
    "visits |>\n  dplyr::count(region, sort = TRUE)",
    "visits |>\n  dplyr::summarise(median = median(wait_min, na.rm = TRUE))"
  )
)

# Visits a week by region, weeks 39 to 46 (section "output").
book_regions <- data.frame(
  region = c("North", "Coast", "Hills"),
  total = c(4751, 2586, 1027),
  latest = c(688, 362, 151),
  change = c("+34%", "+20%", "+26%")
)
book_weeks <- list(
  North = c(512, 548, 530, 601, 577, 640, 655, 688),
  Coast = c(301, 288, 315, 297, 342, 330, 351, 362),
  Hills = c(120, 98, 131, 140, 127, 133, 127, 151)
)

# Two support tickets; the second has no owner yet (section "templates").
book_tickets <- list(
  "SUP-2291" = list(
    key = "SUP-2291",
    title = "Booking page times out",
    status = "Blocked",
    tone = "warning",
    owner = "R. Okafor",
    region = "Coast",
    note = "Waits on the payment provider."
  ),
  "SUP-2304" = list(
    key = "SUP-2304",
    title = "Reply template has a typo",
    status = "New",
    tone = "info",
    owner = NULL,
    region = "North",
    note = "Reported by two readers this week."
  )
)

# Three branches and their week (section "tables").
book_branches <- data.frame(
  branch = c("Harbour Street", "Mill Lane", "Station Road"),
  visits = c(1204, 866, 1512),
  wait_min = c(212, 301, 188),
  opened = c(2019, 2022, 2015),
  staff = c(6, 4, 9)
)

# Four queues of a support desk, and the query behind each (sections 6.2 and 6.3).
book_queues <- data.frame(
  queue = c("Billing", "Booking", "Account", "Other"),
  open = c(42, 118, 27, 9),
  wait_min = c(95, 212, 64, 40),
  owner = c("R. Okafor", "L. Perrin", "A. Haddad", "unassigned"),
  query = c(
    "subset(tickets, queue == 'Billing')",
    "subset(tickets, queue == 'Booking')",
    "subset(tickets, queue == 'Account')",
    "subset(tickets, queue == 'Other')"
  )
)

# How the median wait is counted: the long form of its tip (sections 3.2 and 4.2).
book_wait_more <- paste(
  "**How it is counted.** Tickets opened Monday 00:00 to Sunday 23:59, UTC. The clock runs in business hours",
  "only: 08:00 to 18:00 local time, Monday to Friday.\n\nAuto-closed spam (3.1% of tickets) is left out. A reply",
  "from a bot does not stop the clock; a reply from a person does.\n\n**Source.** The helpdesk export, refreshed",
  "every hour."
)
