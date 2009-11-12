all:
	@test -d ebin || mkdir ebin
	@cp src/oauth.app ebin/
	@erl -make

clean:
	@rm -rf ebin/* erl_crash.dump
