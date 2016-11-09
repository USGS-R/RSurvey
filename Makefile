# prepare the package for release

PKGNAME := $(shell sed -n "s/Package: *\([^ ]*\)/\1/p" DESCRIPTION)
PKGVERS := $(shell sed -n "s/Version: *\([^ ]*\)/\1/p" DESCRIPTION)
PKGSRC  := $(shell basename `pwd`)

all: install check clean

build:
	cd ..;\
	R CMD build $(PKGSRC);\

install: build
	cd ..;\
	R CMD INSTALL $(PKGNAME)_$(PKGVERS).tar.gz;\

check:
	cd ..;\
	R CMD check --as-cran $(PKGNAME)_$(PKGVERS).tar.gz;\

clean:
	cd ..;\
	$(RM) -r $(PKGNAME).Rcheck/;\

.PHONY: all build install check clean
