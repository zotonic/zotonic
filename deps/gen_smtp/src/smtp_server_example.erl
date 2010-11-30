-module(smtp_server_example).
-behaviour(gen_smtp_server_session).

-export([init/4, handle_HELO/2, handle_EHLO/3, handle_MAIL/2, handle_MAIL_extension/2,
	handle_RCPT/2, handle_RCPT_extension/2, handle_DATA/4, handle_RSET/1, handle_VRFY/2,
	handle_other/3, handle_AUTH/4, code_change/3, terminate/2]).

-define(RELAY, true).

-record(state,
	{
		options = [] :: list()
	}).

-spec init(Hostname :: binary(), SessionCount :: non_neg_integer(), Address :: tuple(), Options :: list()) -> {'ok', string(), #state{}} | {'stop', any(), string()}.
init(Hostname, SessionCount, Address, Options) ->
	io:format("peer: ~p~n", [Address]),
	case SessionCount > 20 of
		false ->
			Banner = io_lib:format("~s ESMTP smtp_server_example", [Hostname]),
			State = #state{options = Options},
			{ok, Banner, State};
		true ->
			io:format("Connection limit exceeded~n"),
			{stop, normal, io_lib:format("421 ~s is too busy to accept mail right now", [Hostname])}
	end.

-spec handle_HELO(Hostname :: binary(), State :: #state{}) -> {'error', string(), #state{}} | {'ok', pos_integer(), #state{}} | {'ok', #state{}}.
handle_HELO(<<"invalid">>, State) ->
	% contrived example
	{error, "554 invalid hostname", State};
handle_HELO(<<"trusted_host">>, State) ->
	{ok, State};
handle_HELO(Hostname, State) ->
	io:format("HELO from ~s~n", [Hostname]),
	{ok, 655360, State}. % 640kb of HELO should be enough for anyone.
	%If {ok, State} was returned here, we'd use the default 10mb limit

-spec handle_EHLO(Hostname :: binary(), Extensions :: list(), State :: #state{}) -> {'error', string(), #state{}} | {'ok', list(), #state{}}.
handle_EHLO(<<"invalid">>, _Extensions, State) ->
	% contrived example
	{error, "554 invalid hostname", State};
handle_EHLO(Hostname, Extensions, State) ->
	io:format("EHLO from ~s~n", [Hostname]),
	% You can advertise additional extensions, or remove some defaults
	MyExtensions = case proplists:get_value(auth, State#state.options, false) of
		true ->
			% auth is enabled, so advertise it
			Extensions ++ [{"AUTH", "PLAIN LOGIN CRAM-MD5"}, {"STARTTLS", true}];
		false ->
			Extensions
	end,
	{ok, MyExtensions, State}.

-spec handle_MAIL(From :: binary(), State :: #state{}) -> {'ok', #state{}} | {'error', string(), #state{}}.
handle_MAIL(From, State) ->
	io:format("Mail from ~s~n", [From]),
	% you can accept or reject the FROM address here
	{ok, State}.

-spec handle_MAIL_extension(Extension :: binary(), State :: #state{}) -> {'ok', #state{}} | 'error'.
handle_MAIL_extension(Extension, State) ->
	io:format("Mail from extension ~s~n", [Extension]),
	% any MAIL extensions can be handled here
	{ok, State}.

-spec handle_RCPT(To :: binary(), State :: #state{}) -> {'ok', #state{}} | {'error', string(), #state{}}.
handle_RCPT(To, State) ->
	io:format("Mail to ~s~n", [To]),
	% you can accept or reject RCPT TO addesses here, one per call
	{ok, State}.

-spec handle_RCPT_extension(Extension :: binary(), State :: #state{}) -> {'ok', #state{}} | 'error'.
handle_RCPT_extension(Extension, State) ->
	% any RCPT TO extensions can be handled here
	io:format("Mail to extension ~s~n", [Extension]),
	{ok, State}.

-spec handle_DATA(From :: binary(), To :: [binary(),...], Data :: binary(), State :: #state{}) -> {'ok', string(), #state{}} | {'error', string(), #state{}}.
handle_DATA(From, To, Data, State) ->
	% some kind of unique id
	Reference = lists:flatten([io_lib:format("~2.16.0b", [X]) || <<X>> <= erlang:md5(term_to_binary(erlang:now()))]),
	% if RELAY is true, then relay email to email address, else send email data to console
	case proplists:get_value(relay, State#state.options, false) of
		true -> relay(From, To, Data);
		false ->
			io:format("message from ~s to ~p queued as ~s, body length ~p~n", [From, To, Reference, byte_size(Data)]),
			case proplists:get_value(parse, State#state.options, false) of
				false -> ok;
				true ->
					try mimemail:decode(Data) of
						_Result ->
							io:format("Message decoded successfully!~n")
					catch
						What:Why ->
							io:format("Message decode FAILED with ~p:~p~n", [What, Why]),
							case proplists:get_value(dump, State#state.options, false) of
							false -> ok;
							true ->
								%% optionally dump the failed email somewhere for analysis
								File = "dump/"++Reference,
								case filelib:ensure_dir(File) of
									ok ->
										file:write_file(File, Data);
									_ ->
										ok
								end
							end
					end
			end
	end,
	% At this point, if we return ok, we've accepted responsibility for the email
	{ok, Reference, State}.

-spec handle_RSET(State :: #state{}) -> #state{}.
handle_RSET(State) ->
	% reset any relevant internal state
	State.

-spec handle_VRFY(Address :: binary(), State :: #state{}) -> {'ok', string(), #state{}} | {'error', string(), #state{}}.
handle_VRFY(_Address, State) ->
	{error, "252 VRFY disabled by policy, just send some mail", State}.

-spec handle_other(Verb :: binary(), Args :: binary(), #state{}) -> {string(), #state{}}.
handle_other(Verb, _Args, State) ->
	% You can implement other SMTP verbs here, if you need to
	{lists:flatten(io_lib:format("500 Error: command not recognized : '~s'", [Verb])), State}.

%% this callback is OPTIONAL
%% it only gets called if you add AUTH to your ESMTP extensions
-spec handle_AUTH(Type :: 'login' | 'plain' | 'cram-md5', Username :: binary(), Password :: binary() | {binary(), binary()}, #state{}) -> {'ok', #state{}} | 'error'.
handle_AUTH(Type, <<"username">>, <<"PaSSw0rd">>, State) when Type =:= login; Type =:= plain ->
	{ok, State};
handle_AUTH('cram-md5', <<"username">>, {Digest, Seed}, State) ->
	case smtp_util:compute_cram_digest(<<"PaSSw0rd">>, Seed) of
		Digest ->
			{ok, State};
		_ ->
			error
	end;
handle_AUTH(_Type, _Username, _Password, _State) ->
	error.

-spec code_change(OldVsn :: any(), State :: #state{}, Extra :: any()) -> {ok, #state{}}.
code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

-spec terminate(Reason :: any(), State :: #state{}) -> {'ok', any(), #state{}}.
terminate(Reason, State) ->
	{ok, Reason, State}.

%%% Internal Functions %%%

relay(_, [], _) ->
	ok;
relay(From, [To|Rest], Data) ->
	% relay message to email address
	[_User, Host] = string:tokens(To, "@"),
	gen_smtp_client:send({From, [To], erlang:binary_to_list(Data)}, [{relay, Host}]),
	relay(From, Rest, Data).

