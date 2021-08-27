%% Copyright 2015-2020 Guillaume Bour
%% 
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%% 
%% http://www.apache.org/licenses/LICENSE-2.0
%% 
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

-module(z_letsencrypt_api).
-author("Guillaume Bour <guillaume@bour.cc>").

-export([directory/2, nonce/2, account/4, new_order/5, get_order/4, authorization/4, challenge/4,
         finalize/5, certificate/4, status/1]).

-import(z_letsencrypt_utils, [str/1]).


-ifdef(TEST).
    -define(STAGING_API_URL, <<"https://127.0.0.1:14000/dir">>).
    -define(DEFAULT_API_URL, <<"">>).
-else.
    -define(STAGING_API_URL, <<"https://acme-staging-v02.api.letsencrypt.org/directory">>).
    -define(DEFAULT_API_URL, <<"https://acme-v02.api.letsencrypt.org/directory">>).
-endif.

-ifdef(DEBUG).
    -define(debug(Fmt, Args), io:format(Fmt, Args)).
-else.
    -define(debug(Fmt, Args), ok).
-endif.

-spec status(binary()) -> atom().
status(<<"pending">>)    -> pending;
status(<<"processing">>) -> processing;
status(<<"valid">>)      -> valid;
status(<<"invalid">>)    -> invalid;
status(<<"revoked">>)    -> revoked;
status(_Status)       ->
    io:format("unknown status: ~p~n", [_Status]),
    unknown.

%% PRIVATE

% tcpconn({Prototype, Hostname/IP, Port})
%
% returns: {ok, ConnID}
%
% Opened connections are stored in `conns` ets. If a connection to the given Host:Port
% is already opened, returns it, either open a new connection.
%
% TODO: checks connection is still alive (ping ?)
-spec tcpconn({http|https, string(), integer()}) -> {ok, pid()}.
tcpconn(Key={Proto, Host, Port}) ->
    case ets:info(conns) of
        % does not exists
        undefined -> ets:new(conns, [set, named_table]);
        _         -> ok
    end,

    case ets:lookup(conns, Key) of
        % not found
        [] ->
            %TODO: handle connection failures
            {ok, Conn} = shotgun:open(Host, Port, Proto),
            ets:insert(conns, {Key, Conn}),
            {ok, Conn};
        [{_, Conn}] ->
            {ok, Conn}
    end.

% decode(Option, Result)
%
% Decodes http body as json if asked, or return as if.
%
% returns:
%   {ok, Result} with added json structure if required
%
-spec decode(map(), map()) -> {ok, map()}.
decode(#{json := true}, Response=#{body := Body}) ->
    Payload = jsx:decode(Body, [return_maps]),
    {ok, Response#{json => Payload}};
decode(_, Response) ->
    {ok, Response}.

% request(get|post, Uri, Headers, Content, Options)
%
% Query Uri (get or post) and return results.
%
% returns:
%   {ok, #{status_coe, body, headers}}    :: query succeed
%   {error, invalid_method}               :: Method MUST be either 'get' or 'post'
%   {error, term()}                       :: query failed
%
% TODO: is 'application/jose+json' content type always required ?
%       (check acme documentation)
-spec request(get|post, string()|binary(), map(), nil|binary(), map()) ->
    shotgun:result()|{error, invalid_method}.
request(Method, Uri, Headers, Content, Opts=#{netopts := Netopts}) ->
    #{
        scheme := Proto,
        host := Host,
        port := Port,
        path := Path
    } = uri_string:parse(Uri),

    Headers2 = Headers#{<<"content-type">> => <<"application/jose+json">>},

    % we want to reuse connection if exists
    {ok, Conn} = tcpconn({
        z_convert:to_atom(Proto),
        z_convert:to_list(Host),
        Port}),

    Result = case Method of
        get  -> shotgun:get(Conn, Path, Headers2, Netopts);
        post -> shotgun:post(Conn, Path, Headers2, Content, Netopts)
    end,

    ?debug("~p(~p) => ~p~n", [Method, Uri, Result]),
    case Result of
        {ok, Response=#{headers := RHeaders}} ->
            R = Response#{
                  nonce    => proplists:get_value(<<"replay-nonce">>, RHeaders, nil),
                  location => proplists:get_value(<<"location">>, RHeaders, nil)
            },
            decode(Opts, R);
        _ -> Result
    end.

%%
%% PUBLIC FUNCTIONS
%%

% directory(Environment, Options)
%
% Get directory map listing all acme protocol urls.
% ref: https://www.rfc-editor.org/rfc/rfc8555.html#section-7.1.1
%
% returns:
%   {ok, Directory} where Directory is a map containing protocol urls
%
-spec directory(default|staging, map()) -> {ok, map()}.
directory(Env, Opts) ->
    Uri = case Env of
        staging -> ?STAGING_API_URL;
        _       -> ?DEFAULT_API_URL
    end,
    ?debug("Getting directory at ~p~n", [Uri]),

    {ok, #{json := Directory}} = request(get, Uri, #{}, nil, Opts#{json => true}),
    {ok, Directory}.

% nonce(Directory, Options)
%
% Get a fresh nonce.
% ref: https://www.rfc-editor.org/rfc/rfc8555.html#section-7.2
%
% returns:
%   {ok, Nonce}
%
-spec nonce(map(), map()) -> {ok, binary()}.
nonce(#{<<"newNonce">> := Uri}, Opts) ->
    {ok, #{nonce := Nonce}} = request(get, Uri, #{}, nil, Opts),
    {ok, Nonce}.

% account(Directory, Key, Jws, Opts)
%
% Request new account.
% ref: https://www.rfc-editor.org/rfc/rfc8555.html#section-7.3.1
%
% returns:
%   {ok, Response, Location, Nonce}
%       - Response is json (decoded as map)
%       - Location is create account url
%       - Nonce is a new valid replay-nonce
%
% NOTE: tos are automatically agreed, this should not be the case
% TODO: checks 201 Created response
%
-spec account(map(), z_letsencrypt:ssl_privatekey(), map(), map()) -> {ok, map(), binary(), binary()}.
account(#{<<"newAccount">> := Uri}, Key, Jws, Opts) ->
    Payload = #{
        termsOfServiceAgreed => true,
        contact              => []
    },
    Req = z_letsencrypt_jws:encode(Key, Jws#{url => Uri}, Payload),

    {ok, #{
       json     := Resp,
       location := Location,
       nonce    := Nonce
    }} = request(post, Uri, #{}, Req, Opts#{json => true}),
    {ok, Resp, Location, Nonce}.

% new_order(Directory, Domain, Key, Jws, Opts)
%
% Request new order.
% ref: https://www.rfc-editor.org/rfc/rfc8555.html#section-7.4
%
% returns:
%   {ok, Response, Location, Nonce}
%       - Response is json (decoded as map)
%       - Location is create account url
%       - Nonce is a new valid replay-nonce
%
% TODO: support multiple domains
%       checks 201 created
%
-spec new_order(map(), [ binary() ], z_letsencrypt:ssl_privatekey(), map(), map()) -> {ok, map(), binary(), binary()}.
new_order(#{<<"newOrder">> := Uri}, Domains, Key, Jws, Opts) ->
    Idns = lists:map(
        fun(Domain) ->
            #{
                type => dns,
                value => Domain
            }
        end,
        Domains),
    Payload = #{
        identifiers => Idns
    },
    Req = z_letsencrypt_jws:encode(Key, Jws#{url => Uri}, Payload),

    {ok, #{
        json     := Resp,
        location := Location,
        nonce    := Nonce
    }} = request(post, Uri, #{}, Req, Opts#{json => true}),
    {ok, Resp, Location, Nonce}.

% order(Uri, Key, Jws, Opts)
%
% Get order state.
%
get_order(Uri, Key, Jws, Opts) ->
    % POST-as-GET = no payload
    Req = z_letsencrypt_jws:encode(Key, Jws#{url => Uri}, empty),

    {ok, #{
        json     := Resp,
        location := Location,
        nonce    := Nonce
    }} = request(post, Uri, #{}, Req, Opts#{json=> true}),

    {ok, Resp, Location, Nonce}.

% authorization(Uri, Key, Jws, Opts)
%
% Request authorization for given identifier.
% ref: https://www.rfc-editor.org/rfc/rfc8555.html#section-7.4.1
%
% returns:
%   {ok, Response, Location, Nonce}
%       - Response is json (decoded as map)
%       - Location is create account url
%       - Nonce is a new valid replay-nonce
%
%
-spec authorization(binary(), z_letsencrypt:ssl_privatekey(), map(), map()) -> {ok, map(), binary(), binary()}.
authorization(Uri, Key, Jws, Opts) ->
    % POST-as-GET = no payload
    Req = z_letsencrypt_jws:encode(Key, Jws#{url => Uri}, empty),

    {ok, #{
        json     := Resp,
        location := Location,
        nonce    := Nonce
    }} = request(post, Uri, #{}, Req, Opts#{json=> true}),
    {ok, Resp, Location, Nonce}.

% challenge(Challenge, Key, Jws, Opts)
%
% Notifies acme server we are ready for challenge validation.
% ref: https://www.rfc-editor.org/rfc/rfc8555.html#section-7.5.1
%
% returns:
%   {ok, Response, Location, Nonce}
%       - Response is json (decoded as map)
%       - Location is create account url
%       - Nonce is a new valid replay-nonce
%
-spec challenge(map(), z_letsencrypt:ssl_privatekey(), map(), map()) -> {ok, map(), binary(), binary()}.
challenge(#{<<"url">> := Uri}, Key, Jws, Opts) ->
    % POST-as-GET = no payload
    Req = z_letsencrypt_jws:encode(Key, Jws#{url => Uri}, #{}),

    {ok, #{
       json     := Resp,
       location := Location,
       nonce    := Nonce
    }} = request(post, Uri, #{}, Req, Opts#{json => true}),
    {ok, Resp, Location, Nonce}.

% finalize(Order, Csr, Key, Jws, Opts)
%
% Finalize order once a challenge has been validated.
% ref: https://www.rfc-editor.org/rfc/rfc8555.html#section-7.4
%
% returns:
%
% finalize order
-spec finalize(map(), binary(), z_letsencrypt:ssl_privatekey(), map(), map()) -> {ok, map(), binary(), binary()}.
finalize(#{<<"finalize">> := Uri}, Csr, Key, Jws, Opts) ->
    Payload = #{
        csr => Csr
    },

    Req = z_letsencrypt_jws:encode(Key, Jws#{url => Uri}, Payload),

    {ok, #{
       json     := Resp,
       location := Location,
       nonce    := Nonce
    }} = request(post, Uri, #{}, Req, Opts#{json => true}),
    {ok, Resp, Location, Nonce}.

% certificate(Order, Key, Jws, Opts)
%
% Download certificate (for finalized order.
% ref: https://www.rfc-editor.org/rfc/rfc8555.html#section-7.4.2
%
% returns:
%   {ok, Cert}
%
-spec certificate(map(), z_letsencrypt:ssl_privatekey(), map(), map()) -> {ok, binary()}.
certificate(#{<<"certificate">> := Uri}, Key, Jws, Opts) ->
    % POST-as-GET = no payload
    Req = z_letsencrypt_jws:encode(Key, Jws#{url => Uri}, empty),

    {ok, #{
       body := Cert
    }} = request(post, Uri, #{}, Req, Opts),
    {ok, Cert}.

