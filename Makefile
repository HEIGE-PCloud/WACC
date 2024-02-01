compile:
	stack update
	stack install --local-bin-path . wacc:exe:compile

integration-test: compile
	stack test \
		--test-arguments --catch-stderr \
		--test-arguments --catch-stdout \
		--test-arguments --timeout=1s \
		--test-arguments --num-threads=`nproc` \
		--test-arguments --xml=../rspec.xml \
		--test-arguments '--pattern "$$0 !~ /semanticErr/"'

unit-test:
	stack test \
		--test-arguments --num-threads=`nproc` \
		--test-arguments --xml=../rspec.xml \
	    --test-arguments '--pattern "$$0 !~ /integrationTests/"'

clean:
	$(RM) compile

.PHONY: clean check compile