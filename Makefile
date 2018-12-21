# 今までの影響で `make` を叩いてしまう人が多そうなので `make` で `rebar3` のコマンドを促すようにする
all: compile xref eunit ct dialyzer edoc

init:
	@echo 'Warning: Deprecated make target'
	@echo 'Use `./rebar3 compile`'
	@./rebar3 compile

refresh-deps:
	@echo 'Please use `./rebar3 upgrade` or else'

compile:
	@echo 'Warning: Deprecated make target'
	@echo 'Use `./rebar3 compile`'
	@./rebar3 compile

xref:
	@echo 'Warning: Deprecated make target'
	@echo 'Use `./rebar3 xref`'
	@./rebar3 xref

clean:
	@echo 'Warning: Deprecated make target'
	@echo 'Use `./rebar3 clean`'
	@./rebar3 clean

distclean:
	git clean -df

eunit:
	@echo 'Warning: Deprecated make target'
	@echo 'Use `./rebar3 eunit`'
	@./rebar3 eunit

ct:
	@echo 'Warning: Deprecated make target'
	@echo 'Use `./rebar3 ct`'
	@./rebar3 ct

edoc:
	@echo 'Warning: Deprecated make target'
	@echo 'Use `./rebar3 as dev edoc`'
	@./rebar3 as dev edoc

start:
	@echo 'Warning: Deprecated make target'
	@echo 'Use `./rebar3 shell`'
	@./rebar3 shell

.dialyzer.plt:
	@echo 'Please use `./rebar3 dialyzer` or else'

dialyzer:
	@echo 'Warning: Deprecated make target'
	@echo 'Use `./rebar3 dialyzer`'
	@./rebar3 dialyzer

# 互換性維持用
dialyze: dialyzer
