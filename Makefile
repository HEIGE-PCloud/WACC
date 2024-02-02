STACK=stack $(STACK_FLAGS)

compile:
	$(STACK) update
	$(STACK) install --local-bin-path . wacc:exe:compile

integration-test: compile
	$(STACK) test \
		--test-arguments --catch-stderr \
		--test-arguments --catch-stdout \
		--test-arguments --timeout=10s \
		--test-arguments --num-threads=`nproc` \
		--test-arguments --xml=../rspec.xml \
		--test-arguments '--pattern "$$0 ~ /integrationTests/"'

unit-test:
	$(STACK) test \
		--test-arguments --num-threads=`nproc` \
		--test-arguments --timeout=10s \
		--test-arguments --xml=../rspec.xml \
		--test-arguments '--pattern "$$0 !~ /integrationTests/"' \
		--test-arguments --quickcheck-max-size=10

ghci-test:
	$(STACK) ghci --ghci-options -isrc --ghci-options -itest wacc:wacc-test

clean:
	$(RM) compile

.PHONY: clean check compile
