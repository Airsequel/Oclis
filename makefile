.PHONY: help
help: makefile
	@tail -n +4 makefile | grep ".PHONY"


.PHONY: build
build: cli-spec.json


cli-spec.json: oclis-contract.ncl oclis.ncl
	echo '(import "oclis-contract.ncl") & (import "oclis.ncl")' \
	| nickel export --format json > $@
