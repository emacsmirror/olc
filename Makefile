all: olc.elc olc.info

olc.elc: olc.el
	emacs --batch -f batch-byte-compile olc.el

olc.info: olc.texi
	makeinfo -o olc.info olc.texi

.PHONY: test
test:
	( cd test && \
	  emacs -batch \
		-f package-initialize \
		-l ../olc.el \
		-l olctest.el \
		-f olctest-batch-test )
