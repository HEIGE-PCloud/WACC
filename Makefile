STACK=stack $(STACK_FLAGS)

compile:
	$(STACK) update
	$(STACK) install --local-bin-path . wacc:exe:compile

test-all:
	$(STACK) test \
		--test-arguments --hide-successes \
		--test-arguments --catch-stderr \
		--test-arguments --catch-stdout \
		--test-arguments --timeout=1m \
		--test-arguments --num-threads=`nproc` \
		--test-arguments --xml=../rspec.xml \
		--test-arguments '--pattern "$$0 !~ /semanticErr/"' \
		--test-arguments --quickcheck-max-size=10

integration-test: compile
	$(STACK) test \
		--test-arguments --hide-successes \
		--test-arguments --catch-stderr \
		--test-arguments --catch-stdout \
		--test-arguments --timeout=10s \
		--test-arguments --num-threads=`nproc` \
		--test-arguments --xml=../rspec.xml \
		--test-arguments '--pattern "$$0 ~ /integrationTests/ && $$0 !~ /semanticErr/"'

unit-test:
	$(STACK) test \
		--test-arguments --hide-successes \
		--test-arguments --num-threads=`nproc` \
		--test-arguments --timeout=1m \
		--test-arguments --xml=../rspec.xml \
		--test-arguments '--pattern "$$0 ~ /unitTests/"' \
		--test-arguments --quickcheck-max-size=10

# Use `ACCEPT=1 make golden-test` to accept the changes
golden-test:
	$(STACK) test \
		--test-arguments --hide-successes \
		--test-arguments --num-threads=`nproc` \
		--test-arguments --timeout=1s \
		--test-arguments --xml=../rspec.xml \
		--test-arguments '--pattern "$$0 ~ /goldenTests/"' \
		$(if $(ACCEPT), --test-arguments --accept)

ghci-test:
	$(STACK) ghci --ghci-options -isrc --ghci-options -itest wacc:wacc-test

clean:
	$(RM) compile

.PHONY: clean compile ghci-test golden-test integration-test test-all unit-test
