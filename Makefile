# Copyright (C) 2020 David Byers
#
# Author: David Byers <david.byers@liu.se>
# Version: 1.0
# Package-Requires: ((emacs "25.1"))
# Keywords: extensions, lisp
# URL: https://gitlab.liu.se/davby02/olc
#
# This program is free software: you can redistribute it and/or
# modify it under the terms of the GNU General Public License as
# published by the Free Software Foundation, either version 3 of
# the License, or (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
# General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program. If not, see
# <https://www.gnu.org/licenses/>.

all: olc.elc olc.info

check:
	emacs --batch \
		--eval "(setq-default indent-tabs-mode nil)" \
		-f package-initialize \
		-l elisp-lint \
		-f elisp-lint-files-batch \
		--no-check-declare \
		--no-indent \
		olc.el ; \
	rm -f olc-autoloads.el olc-autoloads.el~

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
