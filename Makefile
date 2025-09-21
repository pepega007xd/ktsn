.DEFAULT_GOAL := benchmark
.PHONY: benchmark verifit results results-diff run run-direct clean bp-archive

benchmark:
	pip install bench/
	dune build && dune install
	rm -rf results/*
	systemd-run --user --scope --slice=benchexec -p Delegate=yes \
		benchexec bench/ktsn.xml --numOfThreads 8

RESULTS_DIR := $(or $(word 2,$(MAKECMDGOALS)),results)

verifit:
	rm -rf $(RESULTS_DIR)
	rsync -avz verifit:ktsn/$(RESULTS_DIR) .
	$(MAKE) results $(RESULTS_DIR)


results:
	pip install bench/
	table-generator -x bench/ktsn-results.xml -o $(RESULTS_DIR) $(RESULTS_DIR)/*.xml.bz2
	python3 -m http.server -b 127.0.0.1 8000 -d .. | \
	firefox "localhost:8000/ktsn/$(RESULTS_DIR)"

ALLARGS := $(wordlist 2, $(words $(MAKECMDGOALS)), $(MAKECMDGOALS))
ARGS_WITH_SUFFIX := $(addsuffix /*.xml.bz2, $(ALLARGS))

results-diff:
	rm -rf results-diff/*
	table-generator $(ARGS_WITH_SUFFIX) -o results-diff
	python3 -m http.server -b 127.0.0.1 8000 -d .. | \
	firefox 'localhost:8000/ktsn/results-diff'

FILE := $(word 2, $(MAKECMDGOALS))
ARGS := $(wordlist 3, $(words $(MAKECMDGOALS)), $(MAKECMDGOALS))

run:
	dune build && dune install
	ivette -scf -ulevel=3 $(FILE) -then-replace \
		-sl -sl-msg-key '*' -sl-benchmark-mode -sl-astral-encoding Bitvectors \
		-sl-backend-solver Bitwuzla -sl-edge-deduplication -sl-simple-join $(ARGS)

run-direct:
	dune build && dune install
	ivette -sl -sl-msg-key '*' -sl-benchmark-mode \
		-sl-astral-encoding Bitvectors -sl-backend-solver Bitwuzla \
		-sl-edge-deduplication -sl-simple-join $(ARGS) $(FILE)

%:
	@:

clean:
	rm -rf _build bp/main.pdf bp/template.pdf bp-archive bench/ktsn.egg-info bench/build

bp-archive: clean
	mkdir bp-archive
	cp -r bp src bench test_programs README.md LICENSE dune-project Makefile bp-archive
	mv bp-archive/bp/thesis.pdf bp-archive
	mkdir bp-archive/excel_at_fit
	cp excel_poster/poster.pdf bp-archive/excel_at_fit/poster.pdf
	cp excel_abstract/abstrakt.pdf bp-archive/excel_at_fit/abstract.pdf
