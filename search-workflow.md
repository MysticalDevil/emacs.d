# Search and Replace Workflow

## Goal
Finish "search -> jump -> batch action" in a few keystrokes with Consult + Embark.

## Recommended flow
1. `C-c k` (`consult-ripgrep`) in project root.
2. Narrow/confirm candidate and jump with `RET`.
3. Run `C-.` (`embark-act`) on a candidate for batch actions.

## Common actions
- From `consult-ripgrep` candidate list:
  - `C-.` -> `embark-export` to editable occur buffer.
  - Edit results, then apply changes from the exported buffer.
- Fast in-buffer navigation:
  - `C-c s l` (`consult-line`)
  - `C-c s o` (`consult-outline`)
  - `C-c s m` (`consult-mark`)

## Project-oriented entry points
- `C-c p p`: switch project.
- `C-c p f`: project file finder.
- `C-c p b`: project buffer list.
- `C-c s g`: `consult-git-grep` for git-tracked search.
