APP=moyo
NODE=$(APP)@localhost
DIALYZER_OPTS=-Werror_handling -Wrace_conditions -Wunmatched_returns

LIBS=$(ERL_LIBS):deps

all: init compile xref eunit edoc dialyze

init:
	@./rebar get-deps compile

update:
	@./rebar update-deps compile

compile:
	@./rebar compile skip_deps=true

xref:
	@./rebar xref skip_deps=true

clean:
	@./rebar clean skip_deps=true

eunit:
	@./rebar eunit skip_deps=true

edoc:
	@./rebar doc skip_deps=true

start: compile
	erl -sname $(NODE) -pz ebin $(shell find deps -type d -name ebin 2>/dev/null) -s reloader \
	  -eval 'erlang:display({start_app, $(APP), application:ensure_all_started($(APP))}).'

.dialyzer.plt:
	touch .dialyzer.plt
	ERL_LIBS=$(LIBS) dialyzer --build_plt --plt .dialyzer.plt --apps erts \
	  $(shell ERL_LIBS=$(LIBS) erl -noshell -pa ebin -eval '{ok, _} = application:ensure_all_started($(APP)), [erlang:display(Name) || {Name, _, _} <- application:which_applications(), Name =/= $(APP)], halt().')

dialyze: .dialyzer.plt compile
	ERL_LIBS=$(LIBS) dialyzer --no_native -pa ebin --plt .dialyzer.plt -I deps -r ebin $(DIALYZER_OPTS)
