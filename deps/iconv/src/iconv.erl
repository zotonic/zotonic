-module(iconv).
%%%----------------------------------------------------------------------
%%% File    : iconv.erl
%%% Author  : Torbjorn Tornkvist <tobbe@bluetail.com>
%%% Purpose : iconv support
%%% Created : 23 Mar 2004 by <tobbe@bluetail.com>
%%%
%%% $Id$
%%%----------------------------------------------------------------------
-behaviour(gen_server).
-export([start/0, start_link/0, stop/0, open/2, conv/2, close/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, 
	 terminate/2, code_change/3]).

-record(state, {port}).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%%% op codes
-define(IV_OPEN,    $o).
-define(IV_CONV,    $v).
-define(IV_CLOSE,   $c).

-define(INBUF_SZ, 512).

-define(DRV_NAME, "iconv_drv").
-define(SERVER, ?MODULE).

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
start() ->
    gen_server:start({local, ?SERVER}, ?MODULE, [], []).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

stop() ->
	gen_server:call(?SERVER, stop).

%%open(To, From) -> {ok, ballen};
open(To, From) ->
    gen_server:call(?SERVER, {open, l2b(To), l2b(From)}, infinity).

%%conv(Cd, String) -> {ok, l2b(String)};
conv(Cd, String) when is_binary(Cd) ->
    gen_server:call(?SERVER, {conv, Cd, l2b(String)}, infinity).

%%close(Cd) -> ok;
close(Cd) when is_binary(Cd) ->
    gen_server:call(?SERVER, {close, Cd}, infinity).

%%%----------------------------------------------------------------------
%%% Callback functions from gen_server
%%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%%----------------------------------------------------------------------
init([]) ->
    erl_ddll:start(),
    Path = case code:priv_dir(iconv) of
			{error, _} ->
				case load_path(?DRV_NAME++".so") of
					{error, _} ->
						error;
					{ok, P} ->
						P
				end;
			P ->
				P
		end,

		case Path of
			error ->
				{stop, no_driver};
			Path ->
				case erl_ddll:load_driver(Path, ?DRV_NAME) of
					ok ->
						Port = open_port({spawn, ?DRV_NAME}, [binary]),
						{ok, #state{port = Port}};
					{error, Error} ->
						error_logger:format("Error loading driver: " ++ erl_ddll:format_error(Error), []),
						{stop, bad_driver}
				end
		end.

%%----------------------------------------------------------------------
%% Func: handle_call/3
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------
handle_call({open, To, From}, _, S) ->
    ToLen   = byte_size(To),
    FromLen = byte_size(From),
    Msg = <<?IV_OPEN,ToLen:16,To/binary,FromLen:16,From/binary>>,
    Reply = call_drv(S#state.port, Msg),
    {reply, Reply, S};
%%
handle_call({conv, Cd, Buf}, _, S) ->
    CdLen  = byte_size(Cd),
    BufLen = byte_size(Buf),
    Msg = <<?IV_CONV,CdLen:16,Cd/binary,BufLen:16,Buf/binary>>,
    Reply = call_drv(S#state.port, Msg),
    {reply, Reply, S};

%%
handle_call({close, Cd}, _, S) ->
    CdLen  = byte_size(Cd),
    Msg = <<?IV_CLOSE,CdLen:16,Cd/binary>>,
    Reply = call_drv(S#state.port, Msg),
    {reply, Reply, S};

handle_call(stop, _, S) ->
	{stop, normal, ok,  S}.

call_drv(Port, Msg) ->
    erlang:port_command(Port, [Msg]),
    recv(Port).

recv(Port) ->
    receive
	{Port, ok} ->
	    ok;
	{Port, value, Bin} ->
	    {ok,Bin};
	{Port, error, ErrAtom} ->
	    {error, ErrAtom}
    end.



%%----------------------------------------------------------------------
%% Func: handle_cast/2
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%----------------------------------------------------------------------
%% Func: handle_info/2
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%----------------------------------------------------------------------
%% Func: terminate/2
%% Purpose: Shutdown the server
%% Returns: any (ignored by gen_server)
%%----------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.


code_change(_, _, _) ->
    ok.

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------
load_path(File) ->
	case lists:zf(fun(Ebin) ->
					Priv = Ebin ++ "/../priv/",
					case file:read_file_info(Priv ++ File) of
						{ok, _} -> {true, Priv};
						_ -> false
					end
			end, code:get_path()) of
		[Dir|_] ->
			{ok, Dir};
		[] ->
			error_logger:format("Error: ~s not found in code path\n", [File]),
			{error, enoent}
	end.

l2b(L) when is_list(L)   -> list_to_binary(L);
l2b(B) when is_binary(B) -> B.

-ifdef(TEST).

smtp_session_test_() ->
	{setup,
		fun() ->
				iconv:start()
		end,
		fun(_) ->
				iconv:stop()
		end,
		[
			{"Convert from latin-1 to utf-8",
				fun() ->
						{ok, CD} = iconv:open("utf-8", "ISO-8859-1"),
						?assertEqual({ok, <<"hello world">>}, iconv:conv(CD, "hello world")),
						iconv:close(CD)
				end
			}
		]
	}.


			-endif.
