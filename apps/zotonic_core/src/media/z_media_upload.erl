%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2026 Marc Worrell
%% @doc Reuse accessible CAFS originals before transferring an upload.
%% @end

%% Copyright 2026 Marc Worrell
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

-module(z_media_upload).
-export([check/2, materialize/2, validate_args/2]).
-include_lib("zotonic.hrl").
-include_lib("zotonic_file.hrl").

%% @doc A miss deliberately does not distinguish private or unavailable content.
-spec check(Payload, Context) -> {ok, map()} when Payload :: map(), Context :: z:context().
check(#{<<"digest">> := Hash, <<"size">> := Size, <<"filename">> := Filename}, Context)
        when is_binary(Hash), byte_size(Hash) =:= 64, is_integer(Size), Size > 0,
             is_binary(Filename), byte_size(Filename) > 0, byte_size(Filename) =< 1024 ->
    case m_cafs:is_enabled(Context) andalso allowed(Filename, Size, Context) of
        true ->
            Rows = matching_sources(Hash, Size, Context),
            case Rows of
                [{Id, Source}|_] ->
                    case available(Hash, Context) of
                        true ->
                            Token = z_crypto:pickle({media_upload, z_acl:user(Context),
                                erlang:system_time(second)+3600, Id, Source, Hash, Size, Filename}, Context),
                            {ok, #{needed => false, media_upload_token => Token}};
                        false -> {ok, #{needed => true}}
                    end;
                [] -> {ok, #{needed => true}}
            end;
        false -> {ok, #{needed => true}}
    end;
check(_, _Context) -> {ok, #{needed => true}}.


%% Older checksummed originals can be adopted on demand. Check visibility before
%% migration and query again afterwards: local bytes may reveal a stale digest.
matching_sources(Hash, Size, Context) ->
    case cafs_sources(Hash, Size, Context) of
        [] ->
            Legacy = z_db:q("select m.id from medium m
                where m.digest=$1 and m.size=$2 and m.is_deletable_file
                    and not exists(select 1 from filename_cafs f where f.filename=m.filename)",
                [Hash, Size], Context),
            promote_match(Legacy, Hash, Size, Context);
        Rows -> Rows
    end.

cafs_sources(Hash, Size, Context) ->
    Rows = z_db:q("select m.id, m.filename from medium m
        join filename_cafs f on f.filename=m.filename
        join cafs c on c.hash=f.hash
        where f.hash=$1 and c.size=$2 and c.state='ready'
            and m.is_deletable_file", [Hash, Size], Context),
    [Row || {Id, _} = Row <- Rows, z_acl:rsc_visible(Id, Context)].

promote_match([], _Hash, _Size, _Context) -> [];
promote_match([{Id}|Rest], Hash, Size, Context) ->
    case z_acl:rsc_visible(Id, Context) of
        true ->
            _ = z_media_cafs_migrate:migrate(Id, Context),
            case cafs_sources(Hash, Size, Context) of
                [] -> promote_match(Rest, Hash, Size, Context);
                Rows -> Rows
            end;
        false -> promote_match(Rest, Hash, Size, Context)
    end.

available(Hash, Context) ->
    Path = m_cafs:path(Hash),
    filelib:is_regular(z_path:files_subdir(Path, Context)) orelse
        z_notifier:first(#filestore{action=exists, path=Path}, Context) =:= true.

allowed(Filename, Size, Context) ->
    z_acl:is_allowed(insert, #acl_media{mime=z_media_identify:guess_mime(Filename), size=Size}, Context).

%% @doc Recheck ownership and visibility at use time. Supply an ordinary upload,
%% so sanitizers, antivirus, observers and destination permissions still run.
-spec materialize(Value, Context) -> {ok, #upload{}} | {error, term()}
    when Value :: map(), Context :: z:context().
materialize(#{<<"media_upload_token">> := Token}, Context) when is_binary(Token), byte_size(Token) < 8192 ->
    Decoded = try z_crypto:depickle(Token, Context) catch _:_ -> invalid end,
    User = z_acl:user(Context),
    Now = erlang:system_time(second),
    case Decoded of
        {media_upload, User, Expires, Id, Source, Hash, Size, Filename} when Expires > Now ->
            case allowed(Filename, Size, Context) andalso z_acl:rsc_visible(Id, Context)
                andalso z_db:q1("select exists(select 1 from medium m
                    join filename_cafs f on f.filename=m.filename
                    join cafs c on c.hash=f.hash
                    where m.id=$1 and m.filename=$2 and f.hash=$3
                        and c.size=$4 and c.state='ready' and m.is_deletable_file)",
                    [Id, Source, Hash, Size], Context)
            of
                true -> copy_upload(Hash, Filename, Context);
                false -> {error, eacces}
            end;
        _ -> {error, eacces}
    end;
materialize(_, _Context) -> {error, unacceptable}.

copy_upload(Hash, Filename, Context) ->
    {ok, {Monitor, Temp}} = z_tempfile:monitored_new(),
    Result = m_cafs:with_local_file(Hash, fun(File) -> file:copy(File, Temp) end, Context),
    case Result of
        {ok, _} ->
            case z_crypto:hex_sha2_file(Temp) of
                {ok, Hash} -> {ok, #upload{filename=Filename, tmpfile=Temp, tmpmonitor=Monitor}};
                _ -> file:delete(Temp), {error, checksum_mismatch}
            end;
        Error -> file:delete(Temp), Error
    end.

-spec validate_args(Args, Context) -> {ok, list()} | {error, term()}
    when Args :: list(), Context :: z:context().
validate_args(Args, Context) ->
    lists:foldr(fun
        ({Key, #{<<"media_upload_token">> := _} = Value}, {ok, Acc}) ->
            case materialize(Value, Context) of
                {ok, Upload} -> {ok, [{Key, Upload}|Acc]};
                Error -> Error
            end;
        (Arg, {ok, Acc}) -> {ok, [Arg|Acc]};
        (_, Error) -> Error
    end, {ok, []}, Args).
