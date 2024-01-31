compile:
	stack update
	stack install --local-bin-path . wacc:exe:compile

check: compile
	stack test \
		--test-arguments --catch-stderr \
		--test-arguments --catch-stdout \
		--test-arguments --timeout=1s \
		--test-arguments --num-threads=4

clean:
	$(RM) compile

.PHONY: clean check compile