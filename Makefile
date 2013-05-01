.PHONY: clean

emacs-backups = $(strip $(wildcard *~) $(wildcard .*~))
fasls = $(strip $(wildcard *.fasl) $(wildcard *.dx64fsl))

all: pp.xsl

%.xsl: %.xsltxt
	java -jar xsltxt.jar toXSL $*.xsltxt $*.xsl || rm $*.xsl;

clean:
ifneq ($(emacs-backups),)
	rm -f $(emacs-backups)
endif
ifneq ($(fasls),)
	rm -f $(fasls)
endif
