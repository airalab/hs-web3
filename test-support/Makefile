.PHONY: install build compile-contracts deploy test

help: ## Ask for help!
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'

all: install
	@echo prereqs that are newer than install: $?

install: ## Install all dependencies
	npm install;
	bower install

build: ## Builds the application
	./node_modules/.bin/pulp build

compile-contracts: ## Compile contracts
	./node_modules/.bin/pulp build --src-path compile -m Compile --to compile.js && node compile.js --log-level info; rm -f compile.js

deploy: compile-contracts build ## Deploy contracts
	./node_modules/.bin/pulp run

test: compile-contracts ## Test contracts
	./node_modules/.bin/pulp test
