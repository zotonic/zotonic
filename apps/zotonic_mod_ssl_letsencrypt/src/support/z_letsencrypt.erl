%% Copyright 2015-2020 Guillaume Bour
%% Copyright 2020-2021 Marc Worrell
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

-module(z_letsencrypt).
-author("Guillaume Bour <guillaume@bour.cc>").
-behaviour(gen_statem).

-include_lib("zotonic_core/include/zotonic.hrl").

-export([make_cert/2, make_cert_bg/2, get_challenge/0]).
-export([start/1, stop/0, init/1, terminate/3, code_change/4]).
-export([idle/3, pending/3, valid/3, invalid/3, revoked/3, finalize/3]).
-export([callback_mode/0]).

-import(z_letsencrypt_utils, [bin/1, str/1]).

% uri format compatible with shotgun library
% NOTE: currently only support 'http-01' challenge.
-type challenge_type() :: 'http-01'.
-type nonce()          :: binary().
-type jws()            :: #{
                                alg => 'RS256',
                                jwk => map(),
                                nonce => undefined | nonce(),
                                kid => binary(),
                                url => string() | binary()
                            }.

-type key_integer() :: integer() | binary().
-type rsa_private() :: [key_integer()].
-type ssl_privatekey() :: #{
                                raw => rsa_private(),
                                b64 => {binary(), binary()},
                                file => string()
                            }.
-type domain()         :: binary() | string().
-type ssl_csr()        :: binary().

-export_type([
        domain/0,
        ssl_privatekey/0,
        jws/0,
        challenge_type/0,
        nonce/0,
        ssl_csr/0
    ]).

-record(state, {
    % acme environment
    env       = prod                :: staging | prod,
    % acme directory (map operation -> uri)
    directory = undefined           :: undefined | map(),
%    acme_srv = ?DEFAULT_API_URL     :: uri() | string(),
    key_file  = undefined           :: undefined | string(),
    cert_path = "/tmp"              :: string(),

    intermediate_cert = undefined   :: undefined | binary(),

    % state datas
    nonce = undefined               :: undefined | nonce(),
    domain = undefined              :: undefined | binary(),
    sans  = []                      :: list(string()),
    key   = undefined               :: undefined | ssl_privatekey(),
    jws   = undefined               :: undefined | jws(),

    account_key = undefined,
    order = undefined,
    challenges = #{}                :: map() | nil,

    % certificate/csr key file
    cert_key_file = undefined,

    % api options
    opts = #{} :: map()

}).

-type state() :: #state{}.

% start(Args).
%
% Starts letsencrypt service.
%
% returns:
%   {ok, Pid}
%
-spec start(list()) -> {'ok', pid()}|{'error', {'already_started',pid()}}.
start(Args) ->
    gen_statem:start_link({global, ?MODULE}, ?MODULE, Args, []).

% stop().
%
% Stops letsencrypt service.
%
% returns:
%   'ok'
-spec stop() -> 'ok'.
stop() ->
    %NOTE: maintain compatibility with 17.X versions
    %gen_fsm:stop({global, ?MODULE})
    gen_statem:call({global, ?MODULE}, stop).


callback_mode() ->
    state_functions.


%% init(Args).
%%
%% Initialize state machine
%%   - init ssl & jws
%%   - fetch acme directory
%%   - get valid nonce
%%
%% transition:
%%   - 'idle' state
-spec init(list( atom() | {atom(),any()} )) -> {ok, idle, state()}.
init(Args) ->
    State = getopts(Args, #state{}),

    % initialize key & jws
    Key = z_letsencrypt_ssl:private_key(State#state.key_file, State#state.cert_path),
    Jws = z_letsencrypt_jws:init(Key),

    % request directory
    {ok, Directory} = z_letsencrypt_api:directory(State#state.env),

    % get first nonce
    {ok, Nonce} = z_letsencrypt_api:nonce(Directory),

    {ok, idle, State#state{directory=Directory, key=Key, jws=Jws, nonce=Nonce}}.


%%
%% PUBLIC funs
%%


% make_cert(Domain, Opts).
%
% Generates a new certificate for given Domain
%
% params:
%   - Domain: domain name to generate acme certificate for
%   - Opts  : dictionary of options
%           * async (bool): if true, make_cert() blocks until complete and returns
%               generated certificate filename
%                         if false, immediately returns
%           * callback: function executed when async = true once domain certificate
%                       has been successfully generated
% returns:
%   - 'async' if async is set (default)
%   - {error, Err} if something goes bad 😈
%
-spec make_cert(string()|binary(), map()) -> {'ok', #{cert => binary(), key => binary()}}|
                                             {'error','invalid'}|
                                             async.
make_cert(Domain, Opts=#{async := false}) ->
    make_cert_bg(Domain, Opts);
% default to async = true
make_cert(Domain, Opts) ->
    _Pid = proc_lib:spawn(?MODULE, make_cert_bg, [Domain, Opts#{async => true}]),
    async.

-spec make_cert_bg(string()|binary(), map()) -> {'ok', map()}|{'error', 'invalid'}.
make_cert_bg(Domain, Opts=#{async := Async}) ->
    Ret = case gen_statem:call({global, ?MODULE}, {create, bin(Domain), Opts}, 15000) of
        {error, Err} ->
            ?LOG_ERROR(#{
                text => <<"LetsEncrypt error making cert">>,
                in => zotonic_mod_ssl_letsencrypt,
                result => error,
                reason => Err,
                domain => Domain
            }),
            {error, Err};
        ok ->
            case wait_valid(20) of
                ok ->
                    Status = gen_statem:call({global, ?MODULE}, finalize, 15000),
                    case wait_finalized(Status, 20) of
                        {ok, Res} -> {ok, Res};
                        Err -> Err
                    end;
                Error ->
                    gen_statem:cast({global, ?MODULE}, reset),
                    Error
            end
    end,
    case Async of
        true ->
            Callback = maps:get(callback, Opts, fun(_) -> ok end),
            Callback(Ret);
        _ ->
            ok
    end,
    Ret.

% get_challenge().
%
% Returns ongoing challenges with pre-computed thumbprints.
%
% returns:
%   #{Challenge => Thumbrint} if ok,
%   'error' if fails
%
-spec get_challenge() -> error|map().
get_challenge() ->
    case catch gen_statem:call({global, ?MODULE}, get_challenge) of
        % process not started, wrong state, ...
        {'EXIT', _Exc} ->
            %io:format("exc: ~p~n", [Exc]),
            error;

        % challenge #{token => ..., thumbprint => ...}
        C -> C
    end.


%%
%% gen_server API
%%


% state 'idle'
%
% When awaiting for certificate request.
%
% idle(get_challenge) :: nothing done
%
idle({call, From}, get_challenge, State) ->
    {keep_state, State, [ {reply, From, no_challenge} ]};

% idle({create, Domain, Opts}).
%
% Starts a new certificate delivery process.
%  - create new account
%  - create new order (incl
%  - requires authorization (returns challenges list)
%  - initiate choosen challenge
%
% transition:
%  - 'idle' if process failed
%  - 'pending' waiting for challenges to be completes
%
idle({call, From}, {create, Domain, CertOpts}, State=#state{directory=Dir, key=Key, jws=Jws,
                                              nonce=Nonce, opts=Opts}) ->
    % 'http-01' or 'tls-sni-01'
    % TODO: validate type
    ChallengeType = maps:get(challenge, Opts, 'http-01'),
    SANs  = lists:map(
        fun(D) -> bin(D) end,
        maps:get(san, CertOpts, [])),

    {ok, Accnt, Location, Nonce2} = z_letsencrypt_api:account(Dir, Key, Jws#{nonce => Nonce}),
    AccntKey = maps:get(<<"key">>, Accnt),

    Jws2 = #{
        alg   => maps:get(alg, Jws),
        nonce => Nonce2,
        kid   => Location
    },
    %TODO: checks order is ok
    Domains = [ bin(Domain) | SANs ],
    {ok, Order, OrderLocation, Nonce3} = z_letsencrypt_api:new_order(Dir, Domains, Key, Jws2),

    % we need to keep trace of order location
    Order2 = Order#{ <<"location">> => OrderLocation },
    StateAuth = State#state{
        domain = Domain,
        jws = Jws2,
        account_key = AccntKey,
        nonce = Nonce3,
        sans = SANs
    },
    case Order2 of
        #{ <<"type">> := <<"urn:ietf:params:acme:error:", _/binary>> = Type } ->
            ?LOG_ERROR(#{
                text => <<"LetsEncrypt] error for cert delivery">>,
                in => zotonic_mod_ssl_letsencrypt,
                result => error,
                reason => invalid,
                domain => Domain,
                type => Type
            }),
            {next_state, invalid, StateAuth, [ {reply, From, ok} ]};
        #{ <<"authorizations">> := AuthUris } ->
            {ok, Challenges, Nonce4} = authz(ChallengeType, AuthUris, StateAuth),
            StateReply = StateAuth#state{
                order = Order2,
                nonce = Nonce4,
                challenges = Challenges
            },
            {next_state, pending, StateReply, [ {reply, From, ok} ]}
    end;

idle({call, From}, Msg, State) ->
    handle_call(Msg, From, State);
idle(cast, Msg, State) ->
    handle_cast(Msg, State).


% state 'pending'
%
% When challenges are on-the-go.
%
% pending(get_challenge).
%
% Returns list of challenges currently on-the-go with pre-computed thumbprints.
%
pending({call, From}, get_challenge, State=#state{account_key=AccntKey, challenges=Challenges}) ->
    % #{Domain => #{
    %     Token => Thumbprint,
    %     ...
    % }}
    %
    Thumbprints = maps:from_list(lists:map(
        fun(#{<<"token">> := Token}) ->
            {Token, z_letsencrypt_jws:keyauth(AccntKey, Token)}
        end, maps:values(Challenges)
    )),
    {keep_state, State, [ {reply, From, Thumbprints} ]};

% pending(check).
%
% Checks if all challenges are completed.
% Switch to 'valid' state iff all challenges are validated only
%
% transition:
%   - 'pending' if at least one challenge is not complete yet
%   - 'valid' if all challenges are complete
%
%TODO: handle other states explicitely (allowed values are 'invalid', 'deactivated',
%      'expired' and 'revoked'
%
pending({call, From}, _Action, State=#state{order=#{<<"authorizations">> := Authzs}, nonce=Nonce, key=Key, jws=Jws}) ->
    % checking status for each authorization
    {StateName, Nonce2} = lists:foldl(fun(AuthzUri, {Status, InNonce}) ->
        {ok, Authz, _, OutNonce} = z_letsencrypt_api:authorization(AuthzUri, Key, Jws#{nonce => InNonce}),
        Ret = case {Status, maps:get(<<"status">>, Authz)} of
            {valid, <<"valid">>} -> valid;
            {pending, _} -> pending;
            {_, <<"pending">>} -> pending;
            {valid, Status2} ->
                NS = z_letsencrypt_api:status(Status2),
                maybe_log_status(NS, Authz),
                NS;
            {_, _} ->
                Status
        end,
        {Ret, OutNonce}
    end, {valid, Nonce}, Authzs),

    %io:format(":: challenge state -> ~p~n", [Reply]),
    % reply w/ StateName
    {next_state, StateName, State#state{nonce=Nonce2}, [ {reply, From, StateName} ]};

pending({call, From}, Msg, State) ->
    handle_call(Msg, From, State);
pending(cast, Msg, State) ->
    handle_cast(Msg, State).

% state 'valid'
%
% When challenges has been successfully completed.
% Finalize acme order and generate ssl certificate.
%
% returns:
%   Status: order status
%
% transition:
%   state 'finalize'
valid({call, From}, _, State=#state{domain=Domain, sans=SANs, cert_path=CertPath,
                         order=Order, key=Key, jws=Jws, nonce=Nonce}) ->

    %NOTE: keyfile is required for csr generation
    #{file := KeyFile} = z_letsencrypt_ssl:private_key({new, str(Domain) ++ ".key"}, CertPath),

    Csr = z_letsencrypt_ssl:cert_request(Domain, CertPath, SANs),
    {ok, FinOrder, _, Nonce2} = z_letsencrypt_api:finalize(Order, Csr, Key,
                                                         Jws#{nonce => Nonce}),

    State1 = State#state{
        order = FinOrder#{ <<"location">> => maps:get(<<"location">>, Order) },
        cert_key_file = KeyFile,
        nonce = Nonce2
    },
    Status = z_letsencrypt_api:status(maps:get(<<"status">>, FinOrder, nil)),
    maybe_log_status(Status, FinOrder),
    {next_state, finalize, State1, [ {reply, From, Status} ]};

valid({call, From}, Msg, State) ->
    handle_call(Msg, From, State);
valid(cast, Msg, State) ->
    handle_cast(Msg, State).

maybe_log_status(valid, _) -> ok;
maybe_log_status(pending, _) -> ok;
maybe_log_status(processing, _) -> ok;
maybe_log_status(Status, #{
        <<"challenges">> := [
            #{
                <<"error">> := #{
                    <<"detail">> := Detail,
                    <<"type">> := Type
                },
                <<"validationRecord">> := [ #{ <<"hostname">> := Hostname } | _ ]
            } | _
        ]
    }) ->
    ?LOG_ERROR(#{
        text => <<"LetsEncrypt status error returned">>,
        in => zotonic_mod_ssl_letsencrypt,
        result => error,
        status => Status,
        hostname => Hostname,
        detail => Detail,
        type => Type
    });
maybe_log_status(Status, JSON) ->
    ?LOG_ERROR(#{
        text => <<"LetsEncrypt status error in response">>,
        in => zotonic_mod_ssl_letsencrypt,
        result => error,
        status => Status,
        fin_order => JSON
    }).




% state 'invalid'
%
% When order failed, and certificate generation is stopped.
%
invalid({call, From}, check, State) ->
    {keep_state, State, [ {reply, From, invalid}]};
invalid({call, From}, Msg, State) ->
    handle_call(Msg, From, State);
invalid(cast, Msg, State) ->
    handle_cast(Msg, State).

% state 'revoked'
%
% When order failed, and certificate generation is stopped.
%
revoked({call, From}, check, State) ->
    {keep_state, State, [ {reply, From, revoked}]};
revoked({call, From}, Msg, State) ->
    handle_call(Msg, From, State);
revoked(cast, Msg, State) ->
    handle_cast(Msg, State).


% state 'finalize'
%
% When order is being finalized, and certificate generation is ongoing.
%
% finalize(processing)
%
% Wait for certificate generation being complete (order status == 'valid').
%
% returns:
%   Status : order status
%
% transition:
%   state 'processing' : still ongoing
%   state 'valid'      : certificate is ready
finalize({call, From}, processing, State=#state{order=Order, key=Key, jws=Jws, nonce=Nonce}) ->
    {ok, Order2, _, Nonce2} = z_letsencrypt_api:get_order(
        maps:get(<<"location">>, Order, nil),
        Key, Jws#{nonce => Nonce}),
    State1 = State#state{
        order = Order2,
        nonce = Nonce2
    },
    Status = z_letsencrypt_api:status(maps:get(<<"status">>, Order2, nil)),
    maybe_log_status(Status, Order2),
    {keep_state, State1, [ {reply, From, Status} ]};

% finalize(valid)
%
% Download certificate & save into file.
%
% returns;
%   #{key, cert}
%       - Key is certificate private key filename
%       - Cert is certificate PEM filename
%
% transition:
%   state 'idle' : fsm complete, going back to initial state
finalize({call, From}, valid, State=#state{order=Order, domain=Domain, cert_key_file=KeyFile,
                                cert_path=CertPath, key=Key, jws=Jws, nonce=Nonce}) ->
    % download certificate
    {ok, Cert} = z_letsencrypt_api:certificate(Order, Key, Jws#{nonce => Nonce}),
    CertFile   = z_letsencrypt_ssl:certificate(Domain, Cert, CertPath),

    State1 = State#state{ nonce = undefined },
    Reply = {ok, #{key => bin(KeyFile), cert => bin(CertFile)}},
    {next_state, idle, State1, [ {reply, From, Reply} ]};

finalize({call, From}, stop, State) ->
    handle_call(stop, From, State);
finalize(cast, Msg, State) ->
    handle_cast(Msg, State);

% finalize(Status)
%
% Any other order status leads to exception.
%
finalize({call, From}, Status, State) ->
    ?LOG_ERROR(#{
        text => <<"LetsEncrypt unknown finalize status">>,
        in => zotonic_mod_ssl_letsencrypt,
        result => error,
        reason => unknown_status,
        status => Status
    }),
    {keep_state, State, [ {reply, From, {error, Status}} ]}.

%%%
%%%
%%%


handle_cast(reset, State) ->
    %io:format("reset from ~p state~n", [StateName]),
    {next_state, idle, State}.

handle_call(stop, From, State) ->
    {stop_and_reply, normal, {reply, From, ok}, State}.

terminate(_,_,_) ->
    ok.

code_change(_, StateName, State, _) ->
    {ok, StateName, State}.


%%
%% PRIVATE funs
%%


% getopts(Args)
%
% Parse z_letsencrypt:start() options.
%
% Available options are:
%   - staging        : runs in staging environment (running on production either)
%   - key_file       : reuse an existing ssl key
%   - cert_path      : path to read/save ssl certificate, key and csr request
%
% returns:
%   - State (type record 'state') filled with options values
%
% exception:
%   - 'badarg' if unrecognized option
%
-spec getopts(list(atom()|{atom(),any()}), state()) -> state().
getopts([], State) ->
    State;
getopts([staging|Args], State) ->
    getopts(
        Args,
        State#state{env = staging}
    );
getopts([{key_file, KeyFile}|Args], State) ->
    getopts(
        Args,
        State#state{key_file = KeyFile}
    );
getopts([{cert_path, Path}|Args], State) ->
    getopts(
        Args,
        State#state{cert_path = Path}
    );
getopts([Unk|_], _) ->
    ?LOG_WARNING(#{
        text => <<"LetsEncrypt: unknown parameter">>,
        in => zotonic_mod_ssl_letsencrypt,
        result => error,
        reason => badarg,
        parameter => Unk
    }),
    %throw({badarg, io_lib:format("unknown ~p parameter", [Unk])}).
    throw(badarg).

% wait_valid(X).
%
% Loops X time on authorization check until challenges are all validated
% (waits incrementing time between each trial).
%
% returns:
%   - {error, timeout} if failed after X loops
%   - {error, Err} if another error
%   - 'ok' if succeed
%
-spec wait_valid(0..20) -> ok|{error, any()}.
wait_valid(X) ->
    wait_valid(X,X).

-spec wait_valid(0..20, 0..20) -> ok|{error, any()}.
wait_valid(0,_) ->
    {error, timeout};
wait_valid(Cnt,Max) ->
    case gen_statem:call({global, ?MODULE}, check, 15000) of
        valid ->
            ok;
        pending ->
            timer:sleep(500*(Max-Cnt+1)),
            wait_valid(Cnt-1,Max);
        invalid ->
            {error, invalid};
        {_, Err} ->
            {error, Err}
    end.

% wait_finalized(X).
%
% Loops X time on order being finalized
% (waits incrementing time between each trial).
%
% returns:
%   - {error, timeout} if failed after X loops
%   - {error, Err} if another error
%   - {'ok', Response} if succeed
%
-spec wait_finalized(atom(), 0..20) -> {ok, map()}|{error, timeout|any()}.
wait_finalized(Status, X) ->
    wait_finalized(Status,X,X).

-spec wait_finalized(atom(), 0..20, 0..20) -> {ok, map()}|{error, timeout|any()}.
wait_finalized(_, 0, _) ->
    {error, timeout};
wait_finalized(Status, Cnt, Max) ->
    case gen_statem:call({global, ?MODULE}, Status, 15000) of
        {ok, Res} ->
            {ok, Res};
        valid ->
            timer:sleep(500*(Max-Cnt+1)),
            wait_finalized(valid, Cnt-1,Max);
        processing  ->
            timer:sleep(500*(Max-Cnt+1)),
            wait_finalized(processing, Cnt-1,Max);
        {_, Err} ->
            {error, Err};
        Any ->
            Any
    end.

% authz(ChallenteType, AuthzUris, State).
%
% Perform acme authorization and selected challenge initialization.
%
% returns:
%   {ok, Challenges, Nonce}
%   {error, Error, Nonce}
%
-spec authz(challenge_type(), list(binary()), state()) -> {ok, map(), nonce()}.
authz(ChallengeType, AuthzUris, State) ->
    {ok, Challenges, Nonce2} = authz_step1(AuthzUris, ChallengeType, State, #{}),
    {ok, Nonce3} = authz_step2(maps:to_list(Challenges), State#state{nonce=Nonce2}),
    {ok, Challenges, Nonce3}.

% authz_step1(AuthzUris, ChallengeType, State).
%
% Request authorizations.
%
% returns:
%   {ok, Challenges, Nonce}
%       - Challenges is map of Uri -> Challenge, where Challenge is of ChallengeType type
%       - Nonce is a new valid replay-nonce
%
-spec authz_step1(list(binary()), challenge_type(), state(), map()) -> {ok, map(), nonce()}.
authz_step1([], _, #state{nonce=Nonce}, Challenges) ->
    {ok, Challenges, Nonce};
authz_step1([Uri|T], ChallengeType, State=#state{nonce=Nonce, key=Key, jws=Jws}, Challenges) ->
    {ok, Authz, _, Nonce2} = z_letsencrypt_api:authorization(Uri, Key, Jws#{nonce => Nonce}),
    % get challenge
    % map(
    %   type,
    %   url,
    %   token
    % )
    [Challenge] = lists:filter(fun(C) ->
            maps:get(<<"type">>, C, error) =:= bin(ChallengeType)
        end,
        maps:get(<<"challenges">>, Authz)
    ),
    authz_step1(T, ChallengeType, State#state{nonce=Nonce2}, Challenges#{Uri => Challenge}).

% authz_step2(Challenges, State).
%
% 2d part Authorization, executed after challenge initialization.
% Notify acme server we're good to proceed to challenges.
%
-spec authz_step2(list(binary()), state()) -> {ok, nonce()}.
authz_step2([], #state{nonce=Nonce}) ->
    {ok, Nonce};
authz_step2([{_Uri, Challenge}|T], State=#state{nonce=Nonce, key=Key, jws=Jws}) ->
    {ok, _, _, Nonce2 } = z_letsencrypt_api:challenge(Challenge, Key, Jws#{nonce => Nonce}),
    authz_step2(T, State#state{nonce=Nonce2}).

