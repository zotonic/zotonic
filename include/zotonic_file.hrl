%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2013-2014 Marc Worrell
%% @doc File/media interface definitions. See also z_media_request.erl

%% Copyright 2013-2014 Marc Worrell
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

-record(z_file_info, {
        modifiedUTC :: calendar:datetime(),
        mime :: binary(),
        size :: integer(),
        acls :: list(integer()|{module, atom()}),
        encodings :: list()
    }).

-record(filestore, {
        action = upload :: lookup | upload | delete,
        path :: file:filename(),
        filename :: file:filename(),
        mime :: binary() | string()
    }).

%%% @doc Notification to find the filestore credentials
-record(filestore_credentials_lookup, {
        id :: integer(),
        path :: binary()
    }).

-record(filestore_credentials_revlookup, {
        service :: binary(),
        location :: binary()
    }).

%%% @doc Filestore credentials, used for uploading a file to a storage service
-record(filestore_credentials, {
        service :: binary(),
        location :: binary(),
        credentials :: any()
    }).


%% @doc Used internally for all the parts of an encoding.
%% One request can consists of different parts (think concatenated js/css)
-record(part_cache, {
        acl,                    % optional associated resource id or module, for acl checks
        modified,               % modification time for this entry
        cache_pid,              % pid of cache entry
        cache_monitor,          % monitor of cache entry
        size
    }).
-record(part_file, {
        acl,                    % optional associated resource id or module, for acl checks
        modified,               % modification time for this entry
        mtime,                  % modification time of the file
        filepath,               % path to static file
        size                    % size of this file (needed for sendfile)
    }).
-record(part_data, {
        acl,                    % optional associated resource id or module, for acl checks
        modified,               % modification time for this entry
        data                    % binary data
    }).
