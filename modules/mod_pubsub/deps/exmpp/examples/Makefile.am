EXTRA_DIST = echo_client.erl bosh_client.erl

all-local:
	@$(ERL) +B -noshell -pa @top_builddir@/ebin			\
		-eval "case make:all() of				\
		         up_to_date -> halt(0);				\
		         Err ->						\
		           io:format(\"~p~n\", [Err]),			\
		           halt(1)					\
		       end"
