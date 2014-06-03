%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2010 Marc Worrell, 2014 Arjan Scherpenisse
%% @doc Wrapper for Zotonic application environment configuration

%% Copyright 2010 Marc Worrell, 2014 Arjan Scherpenisse
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%% 
%%     http://www.apache.org/licenses/LICENSE-2.0
%% 
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

-module(z_config).
-author("Marc Worrell <marc@worrell.nl>").

%% API export
-export([
         get/1,
         get/2
        ]).


-include_lib("zotonic.hrl").


%%====================================================================
%% API
%%====================================================================


%% @doc Get value from config file (cached)
get(Key) ->
    ?MODULE:get(Key, default(Key)).

%% @doc Get value from config file, returning default value when not set (cached)
get(Key, Default) ->
	case application:get_env(zotonic, Key) of
		undefined ->
			Default;
		{ok, Value} ->
			Value
	end.

default(timezone) -> <<"UTC">>;
default(listen_port) -> 8000;
default(listen_ip) -> any;
default(smtp_verp_as_from) -> false;
default(smtp_no_mx_lookups) -> false;
default(smtp_relay) -> false;
default(smtp_host) -> "localhost";
default(smtp_port) -> 2525;
default(smtp_ssl) -> false;
default(smtp_bounce_ip) -> "127.0.0.1";
default(smtp_bounce_port) -> 2525;
default(smtp_spamd_port) -> 783;
default(smtp_delete_sent_after) -> 240;
default(inet_backlog) -> 500;
default(dbhost) -> "localhost";
default(dbport) -> 5432;
default(dbuser) -> "zotonic";
default(dbpassword) -> "";
default(dbschema) -> "public";
default(use_ua_classifier) -> true;
default(_) -> undefined.
