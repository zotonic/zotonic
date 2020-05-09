%% Zotonic modules always start with 'mod_'
-module(mod_example).

%% The author - also shown in the admin ui
-author("Nomen Nescio <nomen@example.com>").

%% A module can be a 'gen_server', a 'supervisor', or just a module
%% without behaviour.
-behaviour(gen_server).

%% The title of your module
-mod_title("Your module title").

%% A short description, shown in the admin ui
-mod_description("Description what this module does.").

%% Priority, lower is higher prio, 500 is default.
-mod_prio(500).

%% The modules or services this module depends on.
%% This module is only started after the mentioned modules
%% or services are started.
%% List of atoms.
-mod_depends([]).

%% The modules or services this module provides.
%% A module always provides itself ('mod_example' in this case)
%% List of atoms.
-mod_provides([]).


-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/1]).

-include_lib("zotonic_core/include/zotonic.hrl").

-record(state, {
        context :: z:context()
    }).

%% Module API

%% The Args is a proplists with the site config and a context.
%% The {context, z:context()} is added to it so there is
%% an instantiated site context. The site context is
%% authenticated as the admin.
start_link(Args) when is_list(Args) ->
    gen_server:start_link(?MODULE, Args, []).

%% gen_server callbacks

init(Args) ->
    {context, Context} = proplists:lookup(context, Args),
    % Instantiate a new, empty, and anonymous site context.
    {ok, #state{ context = z_context:new(Context) }}.

handle_call(Message, _From, State) ->
    {stop, {unknown_call, Message}, State}.

handle_cast(Message, State) ->
    {stop, {unknown_cast, Message}, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
