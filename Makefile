JAVA = java
XSLTXT = xsltxt.jar

all: pp.xsl

%.xsl: %.xsltxt
	$(JAVA) -jar $(XSLTXT) toXSL $*.xsltxt $*.xsl || rm $*.xsl;
