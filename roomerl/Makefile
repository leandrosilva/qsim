all: compile
	
compile: clean
	cp src/roomerl.app ebin/
	erlc -pa ebin/ -o ebin/ src/*.erl
	
compile_test: compile
	erlc -pa ebin/ -o ebin/ test/*.erl

run:
	erl -pa ebin/ deps/**/ebin/ -sname roomerl -s roomerl -config config/production

run_dev:
	erl -pa ebin/ deps/**/ebin/ -sname roomerl -s roomerl -config config/development

run_test: compile_test
	erl -noshell -pa ebin/ deps/**/ebin/ -s roomerl -run $(MODULE) test -run init stop -config config/test

clean:
	rm -f ebin/*.beam
	rm -f erl_crash.dump
