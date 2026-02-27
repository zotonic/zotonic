%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2021-2026 Marc Worrell
%% @doc Model for uploading files
%% @end

%% Copyright 2021-2026 Marc Worrell
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

-module(m_fileuploader).
-moduledoc("
Model to start uploads, upload a block and delete uploads.



Topics
------

`model/fileuploader/post/new` starts a new fileupload.

The posted message must include the filename and the file size:


```javascript
{
    filename: \"test.jpg\",
    size: 1000
}
```

The upload URL and current status is returned:


```json
{
    \"result\": {
        \"filename\": \"test.jpg\",
        \"is_complete\": false,
        \"missing\": [
            {
                \"size\": 1000,
                \"start\": 0
            }
        ],
        \"name\": \"WZkhXoaMwrK2StUHmdpp\",
        \"received\": 0,
        \"size\": 10,
        \"upload_url\": \"https://zotonic.test:8443/fileuploader/upload/WZkhXoaMwrK2StUHmdpp\"
    },
    \"status\": \"ok\"
}
```

Status `\"error\"` is returned if the name is unknown or any error occured.

`model/fileuploader/post/delete/+name` delete a fileupload.

`model/fileuploader/post/upload/+name/+offset` upload data to a fileupload.

The offset must be an integer and the message payload must be binary data.

`model/fileuploader/get/status/+name` fetch the current fileupload status.

Available Model API Paths
-------------------------

| Method | Path pattern | Description |
| --- | --- | --- |
| `get` | `/status/+name/...` | Return sanitized upload status for `+name`: filename, total size, received bytes, completion flag, missing byte ranges, and absolute upload URL. |
| `post` | `/new` | Start or resume upload `name` for payload `filename` + `size` after media-insert ACL check, then return sanitized upload status metadata. No further lookups. |
| `post` | `/delete/+name` | Stop and remove the upload worker/session identified by `+name` via `z_fileuploader:stop/1`. No further lookups. |
| `post` | `/upload/+name/+offset` | Append a binary chunk to upload `+name` at byte offset `+offset` and return updated sanitized upload status (including remaining ranges). No further lookups. |

`/+name` marks a variable path segment. A trailing `/...` means extra path segments are accepted for further lookups.
").

-export([
    m_get/3,
    m_post/3,

    upload/4,
    status/2
]).

-include_lib("zotonic_core/include/zotonic.hrl").

%% @doc Fetch the status of an upload
-spec m_get( list(), zotonic_model:opt_msg(), z:context() ) -> zotonic_model:return().
m_get([ <<"status">>, Name | Rest ], _Msg, Context) ->
    case status(Name, Context) of
        {ok, Status} ->
            {ok, {Status, Rest}};
        {error, _} = Error ->
            Error
    end;
m_get(_Vs, _Msg, _Context) ->
    {error, unknown_path}.


%% @doc Start a file upload or upload a block to a file uploader
-spec m_post( list( binary() ), zotonic_model:opt_msg(), z:context() ) -> zotonic_model:post_return().
m_post([ <<"new">> ], #{ payload := Payload }, Context) when is_map(Payload) ->
    case Payload of
        #{
            <<"filename">> := Filename,
            <<"size">> := Size
        } when is_binary(Filename), Size >= 0 ->
            Name = maps:get(<<"name">>, Payload, z_ids:id()),
            case start_upload(Name, Filename, Size, Context) of
                {ok, Status} ->
                    {ok, cleanup_status(Status, Context)};
                {error, _} = Error ->
                    Error
            end;
        _ ->
            {error, missing_fileinfo}
    end;
m_post([ <<"delete">>, Name ], _Msg, _Context) ->
    z_fileuploader:stop(Name);
m_post([ <<"upload">>, Name, Offset ], #{ payload := Payload }, Context) when is_binary(Payload) ->
    % Upload a new block to the file uploader
    upload(Name, z_convert:to_integer(Offset), Payload, Context).


-spec upload( binary(), non_neg_integer(), binary(), z:context() ) -> {ok, map()} | {error, term()}.
upload(Name, Offset, Payload, Context) ->
    case z_fileuploader:upload(Name, Offset, Payload) of
        {ok, Status} ->
            {ok, cleanup_status(Status, Context)};
        {error, _} = Error ->
            Error
    end.

-spec status( binary(), z:context() ) -> {ok, map()} | {error, term()}.
status(Name, Context) ->
    case z_fileuploader:status(Name) of
        {ok, Status} ->
            {ok, cleanup_status(Status, Context)};
        {error, _} = Error ->
            Error
    end.

%% @doc Start the upload of a file. Check the mime type and size.
start_upload(Name, Filename, Size, Context) ->
    Mime = z_media_identify:guess_mime(Filename),
    case z_acl:is_allowed(insert, #acl_media{ mime = Mime, size = Size }, Context) of
        true ->
            case z_fileuploader:exists(Name) of
                true ->
                    Status = status(Name, Context),
                    ?LOG_INFO(#{
                        in => zotonic_mod_fileuploader,
                        text => <<"New request for existing file uploader - reusing existing one">>,
                        name => Name,
                        status => Status
                    }),
                    Status;
                false ->
                    ?LOG_INFO(#{
                        in => zotonic_mod_fileuploader,
                        text => <<"New file uploader">>,
                        name => Name,
                        filename => Filename,
                        size => Size
                    }),
                    mod_fileuploader:start_child(Name, Filename, Size, Context)
            end;
        false ->
            {error, eacces}
    end.

%% @doc Hide internal information from the returned status record
cleanup_status(#{
        name := Name,
        filename := Filename,
        is_complete := IsCompleted,
        size := Size,
        received := Received,
        missing := Missing
    }, Context) ->
    ContextNoLang = z_context:set_language('x-default', Context),
    UploadUrl = z_dispatcher:url_for(fileuploader, [ {name, Name} ], ContextNoLang),
    #{
        name => Name,
        filename => Filename,
        size => Size,
        received => Received,
        is_complete => IsCompleted,
        upload_url => z_context:abs_url(UploadUrl, Context),
        missing => [
            #{
                start => A,
                size => B - A + 1
            } || {A,B} <- Missing
        ]
    }.
