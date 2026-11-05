%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2026 Marc Worrell
%% @doc Reconcile local CAFS publication left behind by a failed database commit.
%% Never traverses the legacy archive or downloads remote files.
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

-module(z_cafs_file).
-export([reconcile/1, reconcile_prefix/2]).

-include_lib("zotonic.hrl").
-include_lib("kernel/include/file.hrl").

%% Visit one of 256 first-byte directories every ten minutes. A full pass takes
%% less than two days; old unreferenced bytes are subject to five-week retention.
-spec reconcile(Context) -> ok when Context :: z:context().
reconcile(Context) ->
    Bucket = (erlang:system_time(second) div 600) rem 256,
    Prefix = binary:encode_hex(<<Bucket>>, lowercase),
    reconcile_prefix(Prefix, Context).

-spec reconcile_prefix(Prefix, Context) -> ok
    when Prefix :: binary(), Context :: z:context().
reconcile_prefix(<<A,B>> = Prefix, Context)
    when (A >= $0 andalso A =< $9 orelse A >= $a andalso A =< $f),
         (B >= $0 andalso B =< $9 orelse B >= $a andalso B =< $f) ->
    Dir = z_path:files_subdir(<<"archive/cafs/sha256/", Prefix/binary>>, Context),
    Cutoff = erlang:system_time(second) - 35 * 86400,
    filelib:fold_files(z_convert:to_list(Dir), ".*", true,
        fun(File, ok) -> reconcile_file(File, Cutoff, Context) end, ok).

reconcile_file(File, Cutoff, Context) ->
    case file:read_file_info(File, [raw, {time, posix}]) of
        {ok, #file_info{type=regular, mtime=MTime, size=Size}} when MTime < Cutoff ->
            Name = z_convert:to_binary(filename:basename(File)),
            case filename:extension(File) of
                ".staging" -> file:delete(File), ok;
                _ -> recover_object(Name, Size, Context)
            end;
        _ -> ok
    end.

recover_object(Hash, Size, Context) when byte_size(Hash) =:= 64 ->
    %% Only our atomic publisher creates final hash filenames. An orphan is
    %% registered, then the normal locked GC protocol checks references/pins.
    case re:run(Hash, <<"^[0-9a-f]{64}$">>, [{capture, none}]) of
        match ->
            z_db:q("insert into cafs (hash,size,upload_pending,gc_after)
                values ($1,$2,false,now()) on conflict do nothing", [Hash, Size], Context),
            ok;
        nomatch -> ok
    end;
recover_object(_, _, _) -> ok.
