package = cl-dot
prefix  = $(DESTDIR)/usr/local
datadir = $(prefix)/share/doc/$(package)
infodir = $(prefix)/share/info

MAKEINFO = makeinfo
TEXINDEX = texindex
PDFTEX   = pdftex
INSTALL  = install

SPLIT = --no-split
MAKEINFO_FLAGS = --html --css-ref style.css --no-number-sections $(SPLIT)

manual_SOURCES = manual.texi version.texi $(manual_EXTRA_SOURCES)
manual_EXTRA_SOURCES = test.png test-lr.png test-undir.png test-edges.png
dist_DATA = cl-dot.html manual.pdf style.css $(manual_EXTRA_SOURCES)


all: cl-dot.html cl-dot.info.gz manual.pdf

install: install-data install-info

install-info: cl-dot.info.gz
	$(INSTALL) -d $(infodir)
	$(INSTALL) -m 0644 $^ $(infodir)

install-data: $(dist_DATA)
	$(INSTALL) -d $(datadir)
	$(INSTALL) -m 0644 $^ $(datadir)

clean:
	$(RM) cl-dot.html cl-dot.info cl-dot.info.gz
	$(RM) manual.pdf manual.{cp,fn,vr,tp} version.texi.tmp

real-clean: clean
	$(RM) manual.{log,aux,cps,fns,ky,toc,tps,vrs,pg} version.texi

cl-dot.html: $(manual_SOURCES)
	$(MAKEINFO) $(MAKEINFO_FLAGS) $<

cl-dot.info: $(manual_SOURCES)
	$(MAKEINFO) $(SPLIT) $<

manual.pdf: $(manual_SOURCES)

version.texi: version.texi.tmp
	if cmp $< $@ 2>/dev/null; then $(RM) $<; else mv $< $@; fi

version.texi.tmp: VERSION := $(shell git describe --tags --abbrev=6 HEAD || echo unknown)
version.texi.tmp:
	echo "@set VERSION $(VERSION:cl-dot-%=%)" > $@
.PHONY: version.texi.tmp

%.gz: %
	gzip --best < $< > $@

%.pdf: %.texi
	$(PDFTEX) $<
	$(TEXINDEX) $*.{cp,fn,vr,tp}
	$(PDFTEX) $<

.PHONY: all clean real-clean install install-data install-info
