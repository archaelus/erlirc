all: ${BEAM_FILES} src/TAGS

include Makefile.local

ERL_FILES ?=$(wildcard src/*.erl)
HRL_FILES ?=$(wildcard include/*.hrl)
BEAM_FILES ?=$(subst src/,ebin/,$(subst .erl,.beam,${ERL_FILES}))
MODULES ?=$(subst src/,,$(subst .erl,,${ERL_FILES}))

INCLUDE ?=-I include/ -I ${EUNIT_ROOT}/include ${PROJECT_INCLUDE}
CODEPATH ?=-pa ebin -pz ${EUNIT_ROOT}/ebin/ lib/*/ebin/
CODEPATH_JUNGERL ?=-pz ${JUNGERL_ROOT}/lib/*/ebin/
ERLC_CODEPATH ?=-pa ebin -pz ${EUNIT_ROOT}/ebin/ -pz "lib/*/ebin/"
ERLC_CODEPATH_JUNGERL ?=-pz "${JUNGERL_ROOT}/lib/*/ebin/"
ERLC_FLAGS ?=+debug_info -W -o ebin/

NODE ?=-name ${APP_NAME}@127.0.0.1

release: clean ${BEAM_FILES} test xref dialyzer.report docs releases/${VSN}/${APP_NAME}.tar.gz

.PHONY: info clean docs test xref shell dialyzer.report release shell_args shell_boot

info:
	@echo Beam files: ${BEAM_FILES}
	@echo ERL files: ${ERL_FILES}
	@echo Modules: ${MODULES}
	@echo "io:format(\"~P\", [code:get_path(),200]), receive after 1000 -> timeout end, erlang:halt()." | erl $(CODEPATH)  $(CODEPATH_JUNGERL)

clean:
	@rm -f ebin/*.beam priv/sasl/* priv/sasl.log priv/yaws/logs/*.{log,old,access}
	@find src/ priv/ -iname \*~ | xargs rm -v

ebin/%.beam: src/%.erl ${HRL_FILES}
	@echo $@: erlc ${ERLC_FLAGS} ${ERLC_CODEPATH} ${ERLC_CODEPATH_JUNGERL} ${INCLUDE} $<
	@erlc ${ERLC_FLAGS} ${ERLC_CODEPATH} ${ERLC_CODEPATH_JUNGERL} ${INCLUDE} $<

docs: ${ERL_FILES}
	erl -noshell -run edoc_run application "'$(APP_NAME)'" '"."' '[{def,{vsn,"$(VSN)"}}]'

test: ${BEAM_FILES}
	erl $(CODEPATH) $(CODEPATH_JUNGERL) -config priv/${APP_NAME} -eval "lists:map(fun(A) -> {A,application:start(A)} end, [${APP_DEPS}]), application:load(${APP_NAME}), gproc:start_link(), lists:foreach(fun (M) -> io:fwrite(\"Testing ~p:~n\", [M]), eunit:test(M) end, [`perl -e 'print join(",", qw(${MODULES}));'`])." -s init stop -noshell

xref: ${BEAM_FILES}
	erl $(CODEPATH) $(CODEPATH_JUNGERL) -eval "xref:start(${APP_NAME}), io:fwrite(\"~n~nXref: ~p~n~n\", [xref:d(\"ebin/\")])." -s init stop -noshell

src/TAGS: ${BEAM_FILES}
	erl $(CODEPATH) $(CODEPATH_JUNGERL) -eval "tags:dir(\"src/\", [{outdir, \"src/\"}])." -s init stop -noshell

shell_args:
	@(echo -ne "rr(\"include/*\").\nlists:map(fun(A) -> {A,application:start(A)} end, [${APP_DEPS}]).\napplication:start(${APP_NAME})." | pbcopy)

shell: ${BEAM_FILES} shell_args
	erl ${NODE} -config priv/${APP_NAME} $(CODEPATH) -pz /Users/nem/projects/erlang/oserl/doc/examples $(CODEPATH_JUNGERL)

shell_boot: ${BEAM_FILES}
	erl ${NODE} -config priv/${APP_NAME} $(CODEPATH) -pz /Users/nem/projects/erlang/oserl/doc/examples $(CODEPATH_JUNGERL) -boot releases/${VSN}/${APP_NAME}

dialyzer.report: ${BEAM_FILES}
	@dialyzer ${INCLUDE} -pa /Users/nem/projects/erlang/eunit/ebin -pa /Users/nem/projects/erlang/oserl/ebin -pa /Users/nem/projects/erlang/yaws/ebin -pa /Users/nem/projects/erlang/common_lib/ebin -c ${BEAM_FILES}

releases/${VSN}/${APP_NAME}.boot: ${BEAM_FILES} releases/${VSN}/${APP_NAME}.rel ebin/${APP_NAME}.app priv/${APP_NAME}.config
	erl $(CODEPATH) $(CODEPATH_JUNGERL) -eval 'systools:make_script("releases/${VSN}/${APP_NAME}").' -s init stop -noshell

releases/${VSN}/${APP_NAME}.tar.gz: releases/${VSN}/${APP_NAME}.boot
	erl $(CODEPATH) $(CODEPATH_JUNGERL) -eval 'systools:make_tar("releases/${VSN}/${APP_NAME}", [{path, ["lib/*/ebin"]}]).' -s init stop -noshell
