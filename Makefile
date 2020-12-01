BUILD_DIR := _build
ELVIS := _build/elvis/lib/elvis/_build/default/bin/elvis

.PHONY: all
all: compile elvis check

.PHONY: compile
compile:
	rebar3 compile

.PHONY: check
check: xref dialyzer

.PHONY: dialyzer
dialyzer:
	rebar3 dialyzer

.PHONY: xref
xref:
	rebar3 xref

.PHONY: clean
clean:
	rebar3 clean
	rm -f rebar.lock
	rm -rf $(BUILD_DIR)

.PHONY: elvis
elvis: $(ELVIS)
	$(ELVIS) rock

$(ELVIS):
	rebar3 as elvis compile
	cd _build/elvis/lib/elvis && rebar3 escriptize
