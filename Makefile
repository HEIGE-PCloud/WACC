compile:
	stack install --local-bin-path . wacc:exe:compile

.PHONY: clean
clean:
	$(RM) compile
