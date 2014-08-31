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
FLAGS =   -L src -batch -l workgroups2.el --eval "(ido-mode t)"
FLAGSWG = -L src -batch -l workgroups2.el --eval "(ido-mode t)" --eval "(workgroups-mode 1)"
WGCMD = ${EMACS} $(FLAGSWG) --debug-init --eval

clean:
	find . -name '*.elc' -delete

.PHONY: deps
deps:
	mkdir -p deps;
	if [ ! -f deps/f.el ];               then curl https://raw.githubusercontent.com/rejeep/f.el/master/f.el -o deps/f.el; fi;
	if [ ! -f deps/s.el ];               then curl https://raw.githubusercontent.com/magnars/s.el/master/s.el -o deps/s.el; fi;
	if [ ! -f deps/dash.el ];            then curl https://raw.githubusercontent.com/magnars/dash.el/master/dash.el -o deps/dash.el; fi;
	if [ ! -f deps/anaphora.el ];        then curl https://raw.githubusercontent.com/rolandwalker/anaphora/master/anaphora.el -o deps/anaphora.el; fi;
	if [ ! -f deps/magit.el ];           then curl https://raw.githubusercontent.com/magit/magit/master/magit.el -o deps/magit.el; fi;
	if [ ! -f deps/git-commit-mode.el ]; then curl https://raw.githubusercontent.com/magit/git-modes/master/git-commit-mode.el -o deps/git-commit-mode.el; fi;
	if [ ! -f deps/git-rebase-mode.el ]; then curl https://raw.githubusercontent.com/magit/git-modes/master/git-rebase-mode.el -o deps/git-rebase-mode.el; fi;
	if [ ! -f deps/magit-key-mode.el ];  then curl https://raw.githubusercontent.com/magit/magit/master/magit-key-mode.el -o deps/magit-key-mode.el; fi;


docs:
	cd doc && make html

.PHONY: test
test: $(ELCS)
	@$(BATCHE) "(progn\
	(require 'cl) \
	(require 'ert) \
	(put 'flet 'byte-obsolete-info nil))" \
	-l tests/ert-my-utils.el -l tests/workgroups2-tests.el -f ert-run-tests-batch-and-exit


.PHONY: testgui
testgui: $(ELCS)
	@$(NOBATCHE) "(progn\
	(require 'cl) \
	(require 'ert) \
	(put 'flet 'byte-obsolete-info nil))" \
	-l tests/ert-my-utils.el -l tests/workgroups2-tests.el -f my-ert-run-tests
	if [ -f /tmp/wg-tests.log ]; then cat /tmp/wg-tests.log; exit 1; fi;
	if [ -f /tmp/wg-tests-ok.log ]; then cat /tmp/wg-tests-ok.log; fi;
