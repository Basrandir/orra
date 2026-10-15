# Orra

Lisp-first structural computing environment experiments.

Run:

```bash
guix shell -m manifest.scm -- ./bin/orra
```

Press `F1` for the current keyboard shortcuts. Help is read-only; use the mouse
wheel, arrows, or Page Up/Down to scroll, and `F1` or `Esc` to return to your work.
Use `Ctrl+S` to save without leaving edit mode. Save/error messages appear at the
bottom of the window; `Ctrl+G` dismisses them.

The header and window title show new/saved/unsaved state. Quitting, loading, or
recovering with unsaved work asks you to save (`S` or `Ctrl+S`), discard (`D`),
or cancel (`Esc`, `Ctrl+G`, or Enter). Failed saves keep the prompt open.

Press `F2` to search and run live commands. Up/Down selects; Enter chooses/runs.
Argument input is literal Lisp data: for `append-paragraph`, enter `"Hello"`.
Use Tab to return to search or Esc/F2 to cancel without changing your editor.

Press `F3` to search notebooks and objects by title, text, or type. Up/Down selects;
Enter jumps without evaluating code. Esc/F3 cancels and preserves your editor.

Press `F4` to show or hide details for the focused object without ending editing.
The inspector opens in the right margin when space permits, or full-width on
small windows. `Shift+F4` or a click gives it keyboard focus; arrows/Page Up/Down
scroll its details. `Esc` returns to your editor, hiding full-width inspection.
It stays hidden until requested and never adds content to document scrolling.

The notebook/section outline appears beside the document when space permits.
`F5` focuses or opens it; Up/Down selects, Left/Right folds branches, and Enter
navigates. `Esc` returns without changing your editor; `F5` again hides it.
On small windows it opens full-width. The mouse wheel scrolls the pane under it.

Optional font override:

```bash
ORRA_FONT=/path/to/font.ttf guix shell -m manifest.scm -- ./bin/orra
```

Test:

```bash
guix shell -m manifest.scm -- \
  sbcl --eval '(require :asdf)' \
       --eval '(asdf:load-asd #P"/home/bassam/src/orra/orra.asd")' \
       --eval '(asdf:load-asd #P"/home/bassam/src/orra/orra-tests.asd")' \
       --eval '(asdf:test-system :orra-tests)'
```

# Inspiration

Emacs, Project Mage, Glamorous Toolkit, Medley Interlisp
