# Orra

Lisp-first structural computing environment experiments.

Run:

```bash
guix shell -m manifest.scm -- ./bin/orra
```

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
