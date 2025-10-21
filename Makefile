.DEFAULT_GOAL := benchmark
.PHONY: benchmark verifit results results-diff run run-direct build clean bp-archive

benchmark: build
	pip install bench/
	rm -rf results/*
	systemd-run --user --scope --slice=benchexec -p Delegate=yes \
		benchexec bench/seal.xml --numOfThreads 8

RESULTS_DIR := $(or $(word 2,$(MAKECMDGOALS)),results)

verifit:
	rm -rf $(RESULTS_DIR)
	rsync -avz verifit:seal/$(RESULTS_DIR) .
	$(MAKE) results $(RESULTS_DIR)


results:
	pip install bench/
	table-generator -x bench/seal-results.xml -o $(RESULTS_DIR) $(RESULTS_DIR)/*.xml.bz2
	python3 -m http.server -b 127.0.0.1 8000 -d .. | \
	firefox "localhost:8000/seal/$(RESULTS_DIR)"

ALLARGS := $(wordlist 2, $(words $(MAKECMDGOALS)), $(MAKECMDGOALS))
ARGS_WITH_SUFFIX := $(addsuffix /*.xml.bz2, $(ALLARGS))

results-diff:
	rm -rf results-diff/*
	table-generator $(ARGS_WITH_SUFFIX) -o results-diff
	python3 -m http.server -b 127.0.0.1 8000 -d .. | \
	firefox 'localhost:8000/seal/results-diff'

FILE := $(word 2, $(MAKECMDGOALS))
ARGS := $(wordlist 3, $(words $(MAKECMDGOALS)), $(MAKECMDGOALS))

run: build
	ivette -scf -ulevel=3 $(FILE) -then-replace \
	-seal -seal-msg-key '*' -seal-svcomp-mode $(ARGS)

run-direct: build
	ivette -seal -seal-msg-key '*' -seal-svcomp-mode $(ARGS) $(FILE)

build:
	dune build && dune install

%:
	@:

clean:
	rm -rf _build bp/main.pdf bp/template.pdf bp-archive bench/seal.egg-info bench/build svcomp.zip

bp-archive: clean
	mkdir bp-archive
	cp -r bp src bench test_programs README.md LICENSE dune-project Makefile bp-archive
	mv bp-archive/bp/thesis.pdf bp-archive
	mkdir bp-archive/excel_at_fit
	cp excel_poster/poster.pdf bp-archive/excel_at_fit/poster.pdf
	cp excel_abstract/abstrakt.pdf bp-archive/excel_at_fit/abstract.pdf

svcomp-archive: clean
	utils/pack.sh
	zip -r svcomp.zip svcomp
