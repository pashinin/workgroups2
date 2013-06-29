# -*- Makefile -*-

EMACS = emacs

TEST_DIR = src
TRAVIS_FILE = .travis.yml

# Compile with noninteractive and relatively clean environment.
BATCHFLAGS = -batch -q --no-site-file

test:
	${EMACS} -L src $(BATCHFLAGS) -f batch-byte-compile $(TEST_DIR)/*.el
# wg-mode-line-string
	${EMACS} -L src -batch -l workgroups-functions.el --eval '(message (wg-mode-line-string))'
