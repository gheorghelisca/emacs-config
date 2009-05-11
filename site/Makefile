
files = $(wildcard *.el)
elc_files = $(files:.el=.elc)

all: $(elc_files)

mail-addons.elc: mail-addons.el
	@echo "Ignoring $<."


%.elc: %.el
	emacs --batch --eval '(byte-compile-file "$<")'

