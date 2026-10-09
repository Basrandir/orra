# Orra

Lisp-first structural computing environment experiments.

Run:

```bash
guix shell -m manifest.scm -- ./bin/orra
```

Press `F1` for the current keyboard shortcuts. Help is read-only; use the mouse
wheel, arrows, or Page Up/Down to scroll, and `F1` or `Esc` to return to your work.

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
