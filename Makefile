compile:
	stack install --local-bin-path . wacc:exe:compile

.PHONY: clean check

check:
	stack test wacc

clean:
	$(RM) compile
