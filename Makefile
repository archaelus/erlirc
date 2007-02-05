APP_NAME=fixme
VSN=0.1

ERL_FILES=$(wildcard src/*.erl)
HRL_FILES=$(wildcard include/*.hrl)
BEAM_FILES=$(subst src/,ebin/,$(subst .erl,.beam,${ERL_FILES}))
MODULES=$(subst src/,,$(subst .erl,,${ERL_FILES}))

INCLUDE=-I include/ -I /Users/nem/projects/erlang/eunit/include -I /Users/nem/projects/erlang/oserl/include -I /Users/nem/projects/erlang/yaws/include -I /Library/DarwinPorts/lib/erlang/lib/stdlib-1.14.2/include -I ../yaws/include
CODEPATH=-pa ebin -pz /Users/nem/projects/erlang/eunit/ebin -pz /Users/nem/projects/erlang/oserl/ebin -pz /Users/nem/projects/erlang/common_lib/ebin -pz /Users/nem/projects/erlang/yaws/ebin
CODEPATH_JUNGERL=-pz /Users/nem/projects/erlang/jungerl/lib/*/ebin/
ERLC_FLAGS=+debug_info -W -o ebin/

NODE=-name ${APP_NAME}@127.0.0.1

APP_DEPS=sasl

all: ${BEAM_FILES}

release: releases/${VSN}/${APP_NAME}.tar.gz

.PHONY: info clean docs test xref shell dialyzer.report release shell_args

info:
	@echo Beam files: ${BEAM_FILES}
	@echo ERL files: ${ERL_FILES}
	@echo Modules: ${MODULES}
	@echo "io:format(\"~P\", [code:get_path(),200]), receive after 1000 -> timeout end, erlang:halt()." | erl $(CODEPATH)  $(CODEPATH_JUNGERL)

clean:
	@rm ebin/*.beam

ebin/%.beam: src/%.erl ${HRL_FILES}
	@echo $@: erlc ${ERLC_FLAGS} ${CODEPATH} ${INCLUDE} $<
	@erlc ${ERLC_FLAGS} ${CODEPATH} ${INCLUDE} $<

docs:
	erl -noshell -run edoc_run application "'$(APP_NAME)'" '"."' '[{def,{vsn,"$(VSN)"}}]'

test: ${BEAM_FILES}
	@echo -e "application:load(${APP_NAME}), lists:foreach(fun (M) -> io:fwrite(\"Testing ~p:~n\", [M]), eunit:test(M) end, [`perl -e 'print join(",", qw(${MODULES}));'`]), init:stop().\n" | erl $(CODEPATH) $(CODEPATH_JUNGERL) -config priv/${APP_NAME}

xref: ${BEAM_FILES}
	@(echo -e "xref:start(${APP_NAME}), io:fwrite(\"~n~nXref: ~p~n~n\", [xref:d(\"ebin/\")]), init:stop().\n" | erl $(CODEPATH) $(CODEPATH_JUNGERL))

shell_args:
	@(echo -ne "lists:map(fun(A) -> {A,application:start(A)} end, [${APP_DEPS}]).\napplication:start(erms)." | pbcopy)

shell: ${BEAM_FILES} shell_args
	erl ${NODE} -config priv/erms $(CODEPATH) -pz /Users/nem/projects/erlang/oserl/doc/examples $(CODEPATH_JUNGERL)

dialyzer.report: ${BEAM_FILES}
	@dialyzer ${INCLUDE} -pa /Users/nem/projects/erlang/eunit/ebin -pa /Users/nem/projects/erlang/oserl/ebin -pa /Users/nem/projects/erlang/yaws/ebin -pa /Users/nem/projects/erlang/common_lib/ebin -c ebin

releases/${VSN}/${APP_NAME}.boot: ${BEAM_FILES} releases/${VSN}/${APP_NAME}.rel ebin/${APP_NAME}app priv/${APP_NAME}.config
	@(echo -e 'systools:make_script("releases/${VSN}/${APP_NAME}"), init:stop().' "\n" | erl $(CODEPATH) $(CODEPATH_JUNGERL))

releases/${VSN}/${APP_NAME}.tar.gz: releases/${VSN}/erms.boot
	@(echo -e 'systools:make_tar("releases/${VSN}/${APP_NAME}", [{path, ["lib/*/ebin"]}]), init:stop().' "\n" | erl $(CODEPATH) $(CODEPATH_JUNGERL))
