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

-export([
    directory/1,
    nonce/1,
    account/3,
    new_order/4,
    get_order/3,
    authorization/3,
    challenge/3,
    finalize/4,
    certificate/3,
    status/1
]).

% -import(z_letsencrypt_utils, [str/1]).

-include_lib("kernel/include/logger.hrl").

-type request_result() :: #{
    body := binary(),
    json => term(),
    nonce := binary() | undefined,
    location := binary() | undefined
}.

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
status(Status)       ->
    ?LOG_ERROR(#{
        text => <<"LetsEncrypt: unknown status">>,
        in => zotonic_mod_ssl_letsencrypt,
        result => error,
        reason => unknown_status,
        status => Status
    }),
    unknown.

%% PRIVATE

-define(TIMEOUT, 30000).

-spec fetch(Method, Uri, Content, Format) -> Result when
    Method :: get | post,
    Uri :: string() | binary(),
    Content :: binary(),
    Format :: body | json,
    Result :: {ok, request_result()} | {error, term()}.
fetch(Method, Uri, Content, Format) ->
    Options = [
        {timeout, ?TIMEOUT},
        {content_type, "application/jose+json"}
    ],
    case z_url_fetch:fetch(Method, Uri, Content, Options) of
        {ok, {_Final, Hs, _Length, Body}} ->
            Response = #{
                nonce => bin(proplists:get_value("replay-nonce", Hs)),
                location => bin(proplists:get_value("location", Hs)),
                body => Body
            },
            decode(Format, Response);
        {error, _} = Error ->
            Error
    end.

bin(undefined) -> undefined;
bin(S) -> z_convert:to_binary(S).

% decode(Option, Result)
%
% Decodes http body as json if asked, or return as if.
%
% returns:
%   {ok, Result} with added json structure if required
%
-spec decode(json | body, request_result()) -> {ok, request_result()}.
decode(json, Response=#{ body := Body }) ->
    Payload = jsx:decode(Body, [return_maps]),
    {ok, Response#{ json => Payload }};
decode(_, Response) ->
    {ok, Response}.

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
-spec directory(default|staging) -> {ok, map()}.
directory(Env) ->
    Uri = case Env of
        staging -> ?STAGING_API_URL;
        _       -> ?DEFAULT_API_URL
    end,
    ?debug("Getting directory at ~p~n", [Uri]),

    {ok, #{ json := Directory }} = fetch(get, Uri, <<>>, json),
    {ok, Directory}.

% nonce(Directory, Options)
%
% Get a fresh nonce.
% ref: https://www.rfc-editor.org/rfc/rfc8555.html#section-7.2
%
% returns:
%   {ok, Nonce}
%
-spec nonce(map()) -> {ok, binary()}.
nonce(#{<<"newNonce">> := Uri}) ->
    {ok, #{ nonce := Nonce }} = fetch(get, Uri, <<>>, body),
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
-spec account(map(), z_letsencrypt:ssl_privatekey(), map()) -> {ok, map(), binary(), binary()}.
account(#{<<"newAccount">> := Uri}, Key, Jws) ->
    Payload = #{
        termsOfServiceAgreed => true,
        contact              => []
    },
    Req = z_letsencrypt_jws:encode(Key, Jws#{url => Uri}, Payload),

    {ok, #{
       json     := Resp,
       location := Location,
       nonce    := Nonce
    }} = fetch(post, Uri, Req, json),
    {ok, Resp, Location, Nonce}.

% new_order(Directory, Domain, Key, Jws)
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
-spec new_order(map(), [ binary() ], z_letsencrypt:ssl_privatekey(), map()) -> {ok, map(), binary(), binary()}.
new_order(#{<<"newOrder">> := Uri}, Domains, Key, Jws) ->
    ?LOG_NOTICE(#{
        text => <<"LetsEncrypt requesting new certificate">>,
        in => zotonic_mod_ssl_letsencrypt,
        domain => Domains,
        type => dns
    }),
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
    }} = fetch(post, Uri, Req, json),
    {ok, Resp, Location, Nonce}.

% order(Uri, Key, Jws)
%
% Get order state.
%
get_order(Uri, Key, Jws) ->
    % POST-as-GET = no payload
    Req = z_letsencrypt_jws:encode(Key, Jws#{url => Uri}, empty),

    {ok, #{
        json     := Resp,
        location := Location,
        nonce    := Nonce
    }} = fetch(post, Uri, Req, json),

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
-spec authorization(binary(), z_letsencrypt:ssl_privatekey(), map()) -> {ok, map(), binary(), binary()}.
authorization(Uri, Key, Jws) ->
    % POST-as-GET = no payload
    Req = z_letsencrypt_jws:encode(Key, Jws#{url => Uri}, empty),

    {ok, #{
        json     := Resp,
        location := Location,
        nonce    := Nonce
    }} = fetch(post, Uri, Req, json),
    {ok, Resp, Location, Nonce}.

% challenge(Challenge, Key, Jws)
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
-spec challenge(map(), z_letsencrypt:ssl_privatekey(), map()) -> {ok, map(), binary(), binary()}.
challenge(#{<<"url">> := Uri}, Key, Jws) ->
    % POST-as-GET = no payload
    Req = z_letsencrypt_jws:encode(Key, Jws#{url => Uri}, #{}),

    {ok, #{
       json     := Resp,
       location := Location,
       nonce    := Nonce
    }} = fetch(post, Uri, Req, json),
    {ok, Resp, Location, Nonce}.

% finalize(Order, Csr, Key, Jws)
%
% Finalize order once a challenge has been validated.
% ref: https://www.rfc-editor.org/rfc/rfc8555.html#section-7.4
%
% returns:
%
% finalize order
-spec finalize(map(), binary(), z_letsencrypt:ssl_privatekey(), map()) -> {ok, map(), binary(), binary()}.
finalize(#{<<"finalize">> := Uri}, Csr, Key, Jws) ->
    Payload = #{
        csr => Csr
    },

    Req = z_letsencrypt_jws:encode(Key, Jws#{url => Uri}, Payload),

    {ok, #{
       json     := Resp,
       location := Location,
       nonce    := Nonce
    }} = fetch(post, Uri, Req, json),
    {ok, Resp, Location, Nonce}.

% certificate(Order, Key, Jws)
%
% Download certificate (for finalized order.
% ref: https://www.rfc-editor.org/rfc/rfc8555.html#section-7.4.2
%
% returns:
%   {ok, Cert}
%
-spec certificate(map(), z_letsencrypt:ssl_privatekey(), map()) -> {ok, binary()}.
certificate(#{<<"certificate">> := Uri}, Key, Jws) ->
    % POST-as-GET = no payload
    Req = z_letsencrypt_jws:encode(Key, Jws#{url => Uri}, empty),

    {ok, #{
       body := Cert
    }} = fetch(post, Uri, Req, body),
    {ok, Cert}.

