# -*- Makefile -*-

EMACS ?= emacs
TEST_DIR = src
TRAVIS_FILE = .travis.yml
EFLAGS ?= -L ../cl-lib -L src -L . -L tests -L deps
BATCH = $(EMACS) $(EFLAGS) -batch -Q
NOBATCH = $(EMACS) --debug-init $(EFLAGS) -Q
NOBATCHE = $(NOBATCH) -eval
BATCHE = $(BATCH) -eval
BATCHFLAGS = -batch -q --no-site-file
FLAGSWG = -L src -batch -l workgroups2.el --eval "(workgroups-mode 1)"
WGCMD = ${EMACS} $(FLAGSWG) --debug-init --eval

clean:
	find . -name '*.elc' -delete

.PHONY: deps
deps:
	mkdir -p deps;
	if [ ! -f deps/dired-sidebar.el ];     then curl https://raw.githubusercontent.com/jojojames/dired-sidebar/master/dired-sidebar.el -o deps/dired-sidebar.el; fi;
	if [ ! -f deps/dired-subtree.el ];     then curl https://raw.githubusercontent.com/Fuco1/dired-hacks/master/dired-subtree.el -o deps/dired-subtree.el; fi;
	if [ ! -f deps/dired-hacks-utils.el ]; then curl https://raw.githubusercontent.com/Fuco1/dired-hacks/master/dired-hacks-utils.el -o deps/dired-hacks-utils.el; fi;


docs:
	cd doc && make html

.PHONY: test
test: $(ELCS) clean
	@$(BATCHE) "(progn\
	(require 'cl-lib) \
	(require 'ert) \
	(put 'flet 'byte-obsolete-info nil))" \
	-l tests/ert-my-utils.el -l tests/workgroups2-tests.el -f ert-run-tests-batch-and-exit


.PHONY: testgui
testgui: $(ELCS)
	@$(NOBATCHE) "(progn\
	(require 'cl-lib) \
	(require 'ert) \
	(put 'flet 'byte-obsolete-info nil))" \
	-l tests/ert-my-utils.el -l tests/workgroups2-tests.el -f my-ert-run-tests
	if [ -f /tmp/wg-tests.log ]; then cat /tmp/wg-tests.log; exit 1; fi;
	if [ -f /tmp/wg-tests-ok.log ]; then cat /tmp/wg-tests-ok.log; fi;
