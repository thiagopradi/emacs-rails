EMACS  = d:/local/emacs/emacs/bin/emacs
ELPATH = -L ~/.emacs.d

ELCC   = $(EMACS) $(ELPATH) -batch -q -f batch-byte-compile
SOURCES = untabify-file.el inflections.el

ELC=$(SOURCES:.el=.elc)

.SUFFIXES: .elc .el

all: $(ELC)

.el.elc:
		$(ELCC)  $<