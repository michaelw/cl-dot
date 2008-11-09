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

manual_SOURCES = manual.texi $(manual_EXTRA_SOURCES)
manual_EXTRA_SOURCES = test.png test-lr.png test-undir.png
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
	$(RM) manual.pdf manual.{cp,fn,vr,tp} 

real-clean: clean
	$(RM) manual.{log,aux,cps,fns,ky,toc,tps,vrs,pg}

cl-dot.html: manual.texi
	$(MAKEINFO) $(MAKEINFO_FLAGS) $<

cl-dot.info: manual.texi
	$(MAKEINFO) $(SPLIT) $<

manual.pdf: $(manual_SOURCES)

%.gz: %
	gzip --best < $< > $@

%.pdf: %.texi
	$(PDFTEX) $<
	$(TEXINDEX) $*.{cp,fn,vr,tp}
	$(PDFTEX) $<

.PHONY: all clean real-clean install install-data install-info
