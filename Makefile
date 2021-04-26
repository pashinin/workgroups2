SHELL = /bin/sh
EMACS ?= emacs

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
test: clean
	$(EMACS) -Q -batch -L src -l tests/workgroups2-tests.el
