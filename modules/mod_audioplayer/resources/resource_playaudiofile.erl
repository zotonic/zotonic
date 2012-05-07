%% @author Hans-Christian Esperer <hc-git@hcesperer.org>
%% @copyright 2012 Hans-Christian ESperer
%% Date: 2012-05-06
%% @doc Integrate an audio player (currently jPlayer) into zotonic

%% Copyright 2012 hc
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

-module(resource_playaudiofile).
-author("Hans-Christian Esperer <hc-git@hcesperer.org>").

-mod_description("Wire playback js to the client").

-include_lib("zotonic.hrl").

-export([render/3]).

render(AudioID, _Args, Context) ->
    ErrorMsg = ?__("Error while loading audio file", Context),
    Result = io_lib:format(script(), [AudioID, AudioID, AudioID, AudioID, AudioID, ErrorMsg, AudioID]),    
    z_context:add_script_page(Result, Context),
    {ok, << >>}.


script() ->
    Nl = << "\n" >>,
    << "$(\"#jplayer~p\").jPlayer({", Nl/binary,
       "ready: function () {", Nl/binary,
       "$(this).jPlayer(\"setMedia\", {", Nl/binary,
       "mp3: \"/media/audio/~p/audio.mp3\",", Nl/binary,
       "});", Nl/binary,
       "document.getElementById('jinterface~p').style.display = '';", Nl/binary,
       "document.getElementById('jloading~p').style.display = 'none';", Nl/binary,
       "},", Nl/binary,
       "swfPath: \"/lib/swf\",", Nl/binary,
       "supplied: \"mp3\",", Nl/binary,
       "solution: \"html, flash\",", Nl/binary,
       "errorAlerts: false,", Nl/binary,
       "error: function(){document.getElementById('jinterface~p').innerHTML='<p>~s</p>';},", Nl/binary,
       "cssSelectorAncestor: \"#jinterface~p\"",
       "});"
    >>.
