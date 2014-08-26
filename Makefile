# -*- Makefile -*-

EMACS ?= emacs
TEST_DIR = src
TRAVIS_FILE = .travis.yml
EFLAGS ?= -L ../cl-lib -L src -L .
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

deps:
	curl https://raw.githubusercontent.com/rejeep/f.el/master/f.el -o f.el
	curl https://raw.githubusercontent.com/magnars/s.el/master/s.el -o s.el
	curl https://raw.githubusercontent.com/magnars/dash.el/master/dash.el -o dash.el
	curl https://raw.githubusercontent.com/rolandwalker/anaphora/master/anaphora.el -o anaphora.el

.PHONY: test
test: $(ELCS)
	@$(BATCHE) "(progn\
	(require 'cl) \
	(require 'ert) \
	(put 'flet 'byte-obsolete-info nil))" \
	-l tests/workgroups2-tests.el -f ert-run-tests-batch-and-exit


.PHONY: testgui
testgui: $(ELCS)
	@$(NOBATCHE) "(progn\
	(require 'cl) \
	(require 'ert) \
	(put 'flet 'byte-obsolete-info nil))" \
	-l tests/workgroups2-tests.el -f my-ert-run-tests
	if [ -f /tmp/wg-tests.log ]; then cat /tmp/wg-tests.log; exit 1; fi;
	if [ -f /tmp/wg-tests-ok.log ]; then cat /tmp/wg-tests-ok.log; fi;

test2:
# desktop-save-mode
	${EMACS} $(FLAGS) --eval "(desktop-save-mode 1)" --eval "(workgroups-mode 1)"

# WGs list length
	${EMACS} $(FLAGSWG) --eval "(message (number-to-string (length (wg-workgroup-list))))"

# show WG name
	${EMACS} $(FLAGSWG) --eval "(message (wg-workgroup-name (wg-current-workgroup)))"

# save session
	${WGCMD} "(wg-save-session)"

test-ido:
	emacs -Q -L src -l cl.el -l ido.el -l workgroups2.el --eval "(ido-mode t)" --eval "(workgroups-mode 1)"
