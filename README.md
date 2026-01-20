# Emacs Org Workflow

This repo contains a personal Emacs config. The notes below document the
Org-based daily log and knowledge/idea workflow implemented in `init.el`.

## Base directory
- Org files live under `~/CABiNET/org2` on macOS (see `my/org-base-directory`
  for OS-specific paths).
- Daily logs are stored at `daily/YYYY/MM/YYYY-MM-DD.org`.

## Daily log lifecycle
- Open or create today's log: `C-c n d` (`my/open-today-daily-log`).
- New files are created from a template and copy several sections from
  yesterday if they exist (Home, deadlines, tasks, task review, Agile,
  reflection, and `* inbox`). See `init.el` for the exact section names.
- `* inbox` (lowercase) is intended to carry over each day and be cleaned
  via the DONE-archiving command below.

## Capture shortcuts
- `C-c c i`: add a new `** INBOX` entry under today's `* inbox`. This ensures
  the daily file exists before inserting.
- `C-c c j`: append a timestamped log entry under `* LOG`, with an optional
  project tag chosen from top-level headings in `projects.org`.
- `C-c c k`: add to `knowledge.org`.
- `C-c c t`: add to `temp.org`.
- `C-c c r`: add to `idea.org`.

## Agenda (daily-only view)
- `C-c a` runs a wrapper that limits `org-agenda` to today's daily log file.
- If the agenda is empty, confirm that today's log contains agenda-relevant
  items (for example, entries with `SCHEDULED` or `DEADLINE`).

## Archiving DONE
- `C-c n a` archives all DONE tasks in the current buffer to
  `archives/task.org`.

## Refile targets
- Use `C-c r` to refile items into:
  - `projects.org`
  - `someday.org`
  - `tips.org`
  - `papers.org`
  - `archive.org`
