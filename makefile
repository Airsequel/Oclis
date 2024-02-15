.PHONY: help
help: makefile
	@tail -n +4 makefile | grep ".PHONY"


.PHONY: build
build: processing-pipeline.svg


.PHONY: test
test:
	@cargo test


processing-pipeline.svg: processing-pipeline.dot
	dot -T svg -o $@ $<
