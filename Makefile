.PHONY: hlint stylish

hlint: ## Run hlint on all haskell projects
	stack exec hlint -- -h .hlint.yaml src test

stylish: ## Run stylish-haskell over all haskell projects
	find ./src -name "*.hs" | xargs stylish-haskell -c ./.stylish_haskell.yaml -i
	find ./test -name "*.hs" | xargs stylish-haskell -c ./.stylish_haskell.yaml -i
