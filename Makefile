compile:
	stack update
	stack install --local-bin-path . wacc:exe:compile

build-test:
	stack update
	stack build --test --no-run-tests

test-all:
	stack test \
		--test-arguments --hide-successes \
		--test-arguments --catch-stderr \
		--test-arguments --catch-stdout \
		--test-arguments --timeout=1m \
		--test-arguments --num-threads=`nproc` \
		--test-arguments --xml=../rspec.xml \
		--test-arguments --quickcheck-max-size=10

integration-test: compile
	stack test \
		--test-arguments --hide-successes \
		--test-arguments --catch-stderr \
		--test-arguments --catch-stdout \
		--test-arguments --timeout=10s \
		--test-arguments --num-threads=`nproc` \
		--test-arguments --xml=../rspec.xml \
		--test-arguments '--pattern "$$0 ~ /integrationTests/"'

unit-test:
	stack test \
		--test-arguments --hide-successes \
		--test-arguments --num-threads=`nproc` \
		--test-arguments --timeout=1m \
		--test-arguments --xml=../rspec.xml \
		--test-arguments '--pattern "$$0 ~ /unitTests/"' \
		--test-arguments --quickcheck-max-size=10

# Use `ACCEPT=1 make golden-test` to accept the changes
golden-test:
	stack test \
		--test-arguments --hide-successes \
		--test-arguments --num-threads=`nproc` \
		--test-arguments --timeout=1s \
		--test-arguments --xml=../rspec.xml \
		--test-arguments '--pattern "$$0 ~ /goldenTests/"' \
		$(if $(ACCEPT), --test-arguments --accept)

ghci-test:
	stack ghci --ghci-options -isrc --ghci-options -itest wacc:wacc-test

clean:
	$(RM) compile

.PHONY: clean compile ghci-test golden-test integration-test test-all unit-test
