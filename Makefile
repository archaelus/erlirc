
ERL_FILES=$(wildcard src/*.erl)
BEAM_FILES=$(subst src/,ebin/,$(subst .erl,.beam,${ERL_FILES}))

all: ${BEAM_FILES}

info:
	@echo Beam files: ${BEAM_FILES}
	@echo ERL files: ${ERL_FILES}

clean:
	@rm ${BEAM_FILES}

ebin/%.beam: src/%.erl
	@echo $@: erlc -W -o ebin/ -I include/ $<
	@erlc -W -o ebin/ -I include/ $<
