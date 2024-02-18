.PHONY: help
help: makefile
	@tail -n +4 makefile | grep ".PHONY"


.PHONY: build
build: processing-pipeline.svg


.PHONY: install
install:
	cargo install --path .


%.json: %.ncl
	nickel export \
		--color=always \
		oclis-contract.ncl \
		--output $@ \
		$<


spec-files := $(wildcard src/*.ncl examples/*.ncl)
spec-files-json := $(spec-files:.ncl=.json)

.PHONY: test
test: $(spec-files-json)
	cargo clippy
	cargo test


processing-pipeline.svg: processing-pipeline.dot
	dot -T svg -o $@ $<


.PHONY: clean
clean:
	rm -f $(spec-files-json)
	rm -f processing-pipeline.svg
	rm -rf .nickel
