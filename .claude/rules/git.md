---
paths:
  - ".github/**"
  - "NEWS.md"
---

# Branches, commits and pull requests

- **Start with an issue,** opened with one of the issue forms: the goal and a "done when" checklist. One issue per
  piece of work.
- **Branch from the default branch** (`main` or `master`) for that issue. Changes reach it only through pull
  requests.
- **Commits:** Conventional Commits, short (`feat:`, `fix:`, `docs:`, `test:`, `ci:`, `refactor:`, `chore:`).
- **Pull requests** use the template. The title is a Conventional Commit, since squash merging makes it the commit
  on the default branch. List the checks you ran and what they printed.
- Don't commit build output (`*.Rcheck/`, `*.tar.gz`, `docs/`) or local settings (`.Renviron`, `.Rhistory`).
- Never force-push, rewrite history that is already pushed, or use `--no-verify`.
- The maintainer merges, once CI is green.

- **Every issue sits under an epic;** only the root issue has no parent. Link a new issue as a sub-issue of the epic it
  belongs to. It gets one type label and one `area:` label.
- **One pull request per issue.** Its body starts with "In brief", says "Closes #n" and lists the checks run, with
  their results. Squash merging makes the title the commit on `main`.
