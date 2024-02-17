.PHONY: help
help: makefile
	@tail -n +4 makefile | grep ".PHONY"


.PHONY: build
build: processing-pipeline.svg


.PHONY: install
install:
	cargo install --path .


.PHONY: test
test:
	cargo clippy
	cargo test


processing-pipeline.svg: processing-pipeline.dot
	dot -T svg -o $@ $<
