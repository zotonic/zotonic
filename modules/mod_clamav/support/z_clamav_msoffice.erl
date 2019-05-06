%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2019 Marc Worrell
%% @doc Additional filtering of MS Office files.

%% Copyright 2019 Marc Worrell
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

-module(z_clamav_msoffice).

-export([
    scan_file/3
]).

scan_file(File, <<"application/vnd.openxmlformats-officedocument.", _/binary>>, Context) ->
    case {z_config:get(clamav_reject_msoffice_external_links, true),
          z_convert:to_bool( m_config:get_value(mod_clamav, reject_msoffice_external_links, false, Context) )}
    of
        {true, _} ->
            scan_1(File);
        {false, true} ->
            scan_1(File);
        _ ->
            ok
    end;
scan_file(_File, _Mime, _Context) ->
    ok.


scan_1(File) ->
    {ok, Data} = file:read_file(File),
    case binary:match(Data, <<"/externalLinks/">>) of
        nomatch ->
            ok;
        {_, _} ->
            {error, av_external_links}
    end.

