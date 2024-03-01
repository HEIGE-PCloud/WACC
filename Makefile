export PATH := $(PWD):$(PATH)

compile:
	git submodule update --init --recursive
	stack install --local-bin-path . wacc:exe:compile

build-test:
	stack build --test --no-run-tests

TIMEOUT ?= 20s

test:
	stack test \
		$(if $(SHOW_ALL),, --test-arguments --hide-successes) \
		--test-arguments --timeout=$(TIMEOUT) \
		--test-arguments --xml=../rspec.xml \
		--test-arguments --quickcheck-max-size=10 \
		--test-arguments '--pattern "$$$(PATTERN)"' \
		$(if $(ACCEPT), --test-arguments --accept) ; pkill -f "[a-z]+\.out"

test-all: compile
	$(MAKE) test PATTERN="0 ~ /./"

unit-test:
	$(MAKE) test PATTERN="0 ~ /unitTest/"

golden-test:
	$(MAKE) test PATTERN="0 ~ /goldenTest/"

integration-test: compile
	$(MAKE) test PATTERN="0 ~ /integrationTest/"

# Frontend Tests

frontend-test: compile
	$(MAKE) test PATTERN="0 ~ /frontend/"

frontend-integration-test: compile
	$(MAKE) test PATTERN="0 ~ /frontend\.integrationTest/"

parser-unit-test:
	$(MAKE) test PATTERN="0 ~ /parser\.unitTest/"

parser-golden-test:
	$(MAKE) test PATTERN="0 ~ /parser\.goldenTest/"

parser-test:
	$(MAKE) test PATTERN="0 ~ /parser/"

scope-unit-test:
	$(MAKE) test PATTERN="0 ~ /scope\.unitTest/"

scope-test:
	$(MAKE) test PATTERN="0 ~ /scope/"

typechecker-unit-test:
	$(MAKE) test PATTERN="0 ~ /typeChecker\.unitTest/"

typechecker-test:
	$(MAKE) test PATTERN="0 ~ /typeChecker/"

# Backend Tests

backend-test: compile
	TIMEOUT=5s $(MAKE) test PATTERN="0 ~ /backend/"

backend-integration-test: compile
	TIMEOUT=5s $(MAKE) test PATTERN="0 ~ /backend\.integrationTest/"

tac-test:
	$(MAKE) test PATTERN="0 ~ /TAC/"

x86-golden-test:
	$(MAKE) test PATTERN="0 ~ /x86\.goldenTest/"

x86-test:
	$(MAKE) test PATTERN="0 ~ /x86/"


ghci-test:
	stack ghci --ghci-options -isrc --ghci-options -itest wacc:wacc-test

clean:
	$(RM) compile
	$(RM) *.s
	$(RM) test
	$(RM) rspec.xml
	$(RM) *.out

.PHONY: clean compile test
