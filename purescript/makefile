.PHONY: help
help: makefile
	@tail -n +4 makefile | grep ".PHONY"


.PHONY: format-code
format-code:
	npx purs-tidy format-in-place "src/**/*.purs" "test/**/*.purs"


.PHONY: test
test: format-code
	npx spago test


.PHONY: clean
clean:
	-rm -rf .spago
	-rm -rf node_modules
	-rm -rf output
	-rm -rf package-lock.json
	-rm -rf spago.lock
