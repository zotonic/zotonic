%% @author Arjan Scherpenisse <arjan@scherpenisse.net>
%% @date 2010-08-19
%% @doc Image clipper bookmarklet

%% Copyright 2010 Arjan Scherpenisse
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

-module(mod_imageclipper).
-author("Arjan Scherpenisse <arjan@scherpenisse.net>").

-mod_title("Image Clipper").
-mod_description("Provides an image clipper bookmarklet for copying images from other webpages into your site.").

-export([init/1]).

-include("zotonic.hrl").

init(Context) ->
    z_datamodel:manage(?MODULE, datamodel(), Context),
    ok.



datamodel() ->
    [
     {categories,
      [
       {clipping, image,
        [{title, <<"Image clipping">>}]
       }
      ]
     }
    ].
