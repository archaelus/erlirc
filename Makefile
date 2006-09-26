
ERL_FILES=$(wildcard src/*.erl)
BEAM_FILES=$(subst src/,ebin/,$(subst .erl,.beam,${ERL_FILES}))

INCLUDE=-I include/ -I /Users/nem/projects/emsg/yaws/include
ERLC_FLAGS=+debug_info -W -o ebin/

all: ${BEAM_FILES}

.PHONY: info clean

info:
	@echo Beam files: ${BEAM_FILES}
	@echo ERL files: ${ERL_FILES}

clean:
	@rm ${BEAM_FILES}

ebin/%.beam: src/%.erl
	@echo $@: erlc ${ERLC_FLAGS} ${INCLUDE} $<
	@erlc ${ERLC_FLAGS} ${INCLUDE} $<
