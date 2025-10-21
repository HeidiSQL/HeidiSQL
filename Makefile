LAZBUILD := $(shell command -v lazbuild)
OPTS := -B --bm=Release
OPTSQT := --ws=qt5
LPI := heidisql.lpi

BIN := out/heidisql
BINGTK := out/gtk2/heidisql
BINQT := out/qt5/heidisql

# Make shell magic to get version from somewhere
#VERSION := shell magic

# Check for environment variable GITHUB
ifeq ($(origin GITHUB), undefined)
    # GITHUB is not set — require secrets.mk
    ifeq (,$(wildcard ./secrets.mk))
        $(error "secrets.mk not found! Please create it first.")
    endif
    include ./secrets.mk
else
    # GITHUB is set — skip including secrets.mk
    $(info GITHUB is set, not loading secrets.mk)
endif

TRANSIFEX_TOKEN ?= help

.PHONY: all clean tx-push tx-pull copy-locale build-mo build-gtk2 run-gtk2 build-qt5 run-qt5 deb-package tar-gtk2 tar-qt5

all: clean tx-pull build-mo build-gtk2 build-qt5 deb-package tar-gtk2 tar-qt5

clean:
	@echo "=== Cleaning"
	@rm -rf bin/lib/x86_64-linux/*
	@rm -f out/gtk2/* out/qt5/*
	@rm -rf deb rpm tar dist

tx-push:
	@echo "=== Pushing to Transifex"
# 	Need to ask Ansgar how he pushes to Transifex

tx-pull:
	@echo "=== Pulling from Transifex"
	@TX_TOKEN=$(TRANSIFEX_TOKEN) ./extra/internationalization/tx pull -a

copy-locale:
	@echo "=== Copying .mo from extra/locale to out/locale"
	@mkdir -p out/locale
	@cp -fv extra/locale/*.mo out/locale

build-mo:
	@echo "=== Building MO files"
	@for file in $(shell find ./extra/locale -iname '*.po'); do \
	  lang=`echo $${file} | cut -d'/' -f4`; \
	  echo "Building MO file for $${lang}"; \
	  msgfmt -o "extra/locale/heidisql.$${lang}.mo" $${file}; \
	done

build-gtk2:
	@echo "=== Building GTK2"
	$(LAZBUILD) $(OPTS) $(LPI)
	mkdir -p out/gtk2
	mv -v $(BIN) $(BINGTK)

run-gtk2: build-gtk2 build-mo
	@echo "=== Running GTK2"
# 	Need to get the .mo files to run

build-qt5:
	@echo "=== Building QT5"
	$(LAZBUILD) $(OPTS) $(OPTSQT) $(LPI)
	mkdir -p out/qt5
	mv -v $(BIN) $(BINQT)

run-qt5: build-qt5 build-mo
	@echo "=== Running GTK2"
# 	Need to get the .mo files to folder run

deb-package: build-mo
	@echo "=== Creating debian package"
	rm -vrf deb
	cp -R package-skeleton deb
	find deb -iname ".gitkeep" -exec rm -v {} +
	cp -vR extra/locale/*.mo deb/usr/share/heidisql/locale
	cp -v extra/ini/*.ini  deb/usr/share/heidisql
	cp -v res/deb-package-icon.png deb/usr/share/pixmaps/heidisql.png
	cp -v $(BINGTK) deb/usr/share/heidisql/heidisql
	chmod +x deb/usr/share/heidisql/heidisql
	cp -v README.md LICENSE deb/usr/share/doc/heidisql
	mkdir -p dist
	rm -vf dist/*.deb
	fpm -s dir -t deb -n heidisql -v ${tag} \
	  -p dist \
	  --verbose \
	  --deb-custom-control deb-control.txt \
	  --deb-no-default-config-files \
	  ./deb/=/

tar-gtk2: build-mo
	@echo "=== Creating GTK2 archive"
	rm -vrf tar
	mkdir -p tar/locale dist
	cp -v README.md LICENSE tar
	cp -v res/deb-package-icon.png tar/heidisql.png
	cp -v extra/locale/*.mo tar/locale
	cp -v extra/ini/*.ini tar
	cp -v out/gtk2/heidisql tar
	chmod +x tar/heidisql
	cd tar && tar -zcvf ../dist/build-gtk2-$(tag).tgz *

tar-qt5: build-mo
	@echo "=== Creating QT5 archive"
	rm -vrf tar
	mkdir -p tar/locale dist
	cp -v README.md LICENSE tar
	cp -v res/deb-package-icon.png tar/heidisql.png
	cp -v extra/locale/*.mo tar/locale
	cp -v extra/ini/*.ini tar
	cp -v out/qt5/heidisql tar
	chmod +x tar/heidisql
	cd tar && tar -zcvf ../dist/build-qt5-$(tag).tgz *
