ASDF_LOAD = (require :asdf)
ORRA_ASD = $(CURDIR)/orra.asd
ORRA_TESTS_ASD = $(CURDIR)/orra-tests.asd
LOAD_ORRA = (asdf:load-asd #P"$(ORRA_ASD)") (asdf:load-system :orra)

.PHONY: run test

run:
	sbcl --eval '$(ASDF_LOAD)' --eval '$(LOAD_ORRA)' --eval '(orra:start-demo)'

test:
	sbcl --eval '$(ASDF_LOAD)' \
	     --eval '(asdf:load-asd #P"$(ORRA_ASD)")' \
	     --eval '(asdf:load-asd #P"$(ORRA_TESTS_ASD)")' \
	     --eval '(asdf:test-system :orra-tests)'
