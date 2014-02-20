# -*- Makefile -*-

EMACS = emacs

TEST_DIR = src
TRAVIS_FILE = .travis.yml

# Compile with noninteractive and relatively clean environment.
BATCHFLAGS = -batch -q --no-site-file
FLAGS = -L src -batch -l ido.el -l workgroups2.el --eval "(ido-mode t)"
FLAGSWG = -L src -batch -l ido.el -l workgroups2.el --eval "(ido-mode t)" --eval "(workgroups-mode 1)"

clean:
	find . -name '*.elc' -delete

test: clean
# just load all files
	${EMACS} -L src $(BATCHFLAGS) -f batch-byte-compile $(TEST_DIR)/*.el

# wg-mode-line-string
	${EMACS} -L src -batch -l workgroups-functions.el --eval '(message (wg-mode-line-string))'

# desktop-save-mode
	${EMACS} $(FLAGS) --eval "(desktop-save-mode 1)" --eval "(workgroups-mode 1)"

# WGs list length
	${EMACS} $(FLAGSWG) --eval "(message (number-to-string (length (wg-workgroup-list))))"

# show WG name
	${EMACS} $(FLAGSWG) --eval "(message (wg-workgroup-name (wg-current-workgroup)))"

test-ido:
	emacs -Q -L src -l cl.el -l ido.el -l workgroups2.el --eval "(ido-mode t)" --eval "(workgroups-mode 1)"
