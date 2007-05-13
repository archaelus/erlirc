# Build the .beam erlang VM files
OPT = -W
INC = ./inc
CC  = erlc
ERL = erl
SED = $(shell which sed)

ESRC = ./src
EBIN = ./ebin

RAW_INSTALL_DIR = /usr/lib/erlang/
ERLANG_INSTALL_DIR = $(DESTDIR)/$(RAW_INSTALL_DIR)/lib
APPLICATION = manderlbot
VERSION = 0.9.2

TARGETDIR = $(ERLANG_INSTALL_DIR)/$(APPLICATION)-$(VERSION)
BINDIR    = $(DESTDIR)/usr/bin
CONFDIR   = $(DESTDIR)/etc/manderlbot
LOGFILE   = $(DESTDIR)/var/log/manderlbot.log

TMP       = $(wildcard *~) $(wildcard src/*~) $(wildcard inc/*~)
INC_FILES = $(wildcard $(INC)/*.hrl)
SRC       = $(wildcard $(ESRC)/*.erl)
CONFFILES = conf/config.xml $(wildcard conf/*fortune)

TARGET   = $(addsuffix .beam, $(basename \
             $(addprefix $(EBIN)/, $(notdir $(SRC)))))
EMAKE    = $(addsuffix \'., $(addprefix \'../, $(SRC)))

SRC_APPFILES   = $(ESRC)/$(APPLICATION).app.src $(ESRC)/$(APPLICATION).rel.src
TGT_APPFILES_E = $(EBIN)/$(APPLICATION).app
TGT_APPFILES_P = priv/$(APPLICATION)*

SCRIPT   = $(BINDIR)/manderlbot
BUILD_OPTIONS =	'[{systools, [{variables,[{"ROOT","$(RAW_INSTALL_DIR)"}]}]}, \
	{sh_script, none}, {report, verbose}, {app_vsn, "$(VERSION)"}, \
        {make_app, true }, {make_rel, true}].'
BUILD_OPTIONS_FILE = ./BUILD_OPTIONS 

.PHONY: doc

manderlbot: $(TARGET)

all: clean manderlbot

# used to generate the erlang Emakefile
emake:
	@echo $(EMAKE) | tr -s ' ' '\n' > ebin/Emakefile

clean:
	-cd priv && rm -f $(shell ls priv | grep -v builder\.erl) && cd ..
	-rm -f $(TARGET) $(TMP) $(BUILD_OPTIONS_FILE) builder.beam
	-rm -f $(TGT_APPFILES)
	-rm -f ebin/*
	-rm -f manderlbot.sh
	-make -C doc clean

install: doc build manderlbot.sh
	-rm -f $(TMP)

	install -d $(TARGETDIR)/priv
	install -d $(TARGETDIR)/ebin
	install -d $(TARGETDIR)/src
	install -d $(TARGETDIR)/include

	cp $(INC_FILES) $(TARGETDIR)/include
	cp $(TARGET) $(TARGETDIR)/ebin

	cp $(TGT_APPFILES_E) $(TARGETDIR)/ebin
	cp $(TGT_APPFILES_P) $(TARGETDIR)/priv

	cp $(SRC) $(SRC_APPFILES) $(TARGETDIR)/src

# install the man page
	install -d $(DESTDIR)/usr/share/man/man1
	install doc/manderlbot.1 $(DESTDIR)/usr/share/man/man1

# create startup script
	cp manderlbot.sh $(SCRIPT)
	chmod +x $(SCRIPT)

# added for debian
	mkdir -p $(CONFDIR)
	cp $(CONFFILES) $(CONFDIR)

uninstall:
	rm -rf $(TARGETDIR) $(SCRIPT)

build: manderlbot builder.beam $(SRC_APPFILES)
# use builder to make boot file
	mkdir -p temp
	ln -sf `pwd` temp/$(APPLICATION)-$(VERSION)
	(cd temp/$(APPLICATION)-$(VERSION) \
	 && echo $(BUILD_OPTIONS) > $(BUILD_OPTIONS_FILE) \
	 && erl -s builder go -s init stop \
	)
	rm -rf temp

doc: 
	make -C doc

release:
	rm -fr $(APPLICATION)-$(VERSION)
	mkdir -p $(APPLICATION)-$(VERSION)
	tar zcf tmp.tgz $(SRC) $(SRC_APPFILES) $(INC_FILES) \
		doc/*.lyx doc/Makefile doc/*.hva doc/*sgml \
		LICENSE README TODO $(CONFFILES) Makefile \
		priv/builder.erl manderlbot.sh.in bofh.fortune
	tar -C $(APPLICATION)-$(VERSION) -zxf tmp.tgz
	mkdir $(APPLICATION)-$(VERSION)/ebin
	tar zvcf  $(APPLICATION)-$(VERSION).tar.gz $(APPLICATION)-$(VERSION)
	rm -fr $(APPLICATION)-$(VERSION)
	rm -fr tmp.tgz

builder.beam: priv/builder.erl 
	$(CC) $(OPT) -I $(INC) $<

ebin/%.beam: src/%.erl 
	$(CC) $(OPT) -I $(INC) -o ebin $<

manderlbot.sh: manderlbot.sh.in Makefile
	@$(SED) \
		-e 's;%INSTALL_DIR%;${RAW_INSTALL_DIR};g' \
		-e 's;%VERSION%;${VERSION};g' < $< > $@

%:%.sh
# Override makefile default implicit rule
