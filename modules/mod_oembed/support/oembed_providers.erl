%% @author Arjan Scherpenisse <arjan@scherpenisse.net>
%% @copyright 2011 Arjan Scherpenisse <arjan@scherpenisse.net>
%% Date: 2011-09-19
%% @doc A list of known oembed providers.

%% Copyright 2011 Arjan Scherpenisse <arjan@scherpenisse.net>
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

-module(oembed_providers).

-export([list/1]).

-include_lib("../include/oembed.hrl").


list(_Context) ->
    [

     #oembed_provider{
       url_re="^https?://blip\\.tv/.+",
       endpoint_url="http://blip.tv/oembed/",
       title="blip.tv"
     },

     #oembed_provider{
       url_re="^https?://dailymotion\\.com/.+",
       endpoint_url="http://www.dailymotion.com/api/oembed/",
       title="Dailymotion"
     },

     #oembed_provider{
       url_re="^https?://.+flickr\\.com/photos/[-.\\w@]+/\\d+/?",
       endpoint_url="http://www.flickr.com/services/oembed/",
       title="Flickr Photos"
     },

     #oembed_provider{
       url_re="^https?://hulu\\.com/watch/.*",
       endpoint_url="http://www.hulu.com/api/oembed.json",
       title="Hulu"
     },

     #oembed_provider{
       url_re="^https?://nfb\\.ca/film/[-\\w]+/?",
       endpoint_url="http://www.nfb.ca/remote/services/oembed/",
       title="National Film Board of Canada"
     },

     #oembed_provider{
       url_re="^https?://qik\\.com/\\w+",
       endpoint_url="http://qik.com/api/oembed.json",
       title="Qik Video"
     },

     #oembed_provider{
       url_re="^https?://.+revision3\\.com/.+",
       endpoint_url="http://revision3.com/api/oembed/",
       title="Revision3"
     },

     #oembed_provider{
       url_re="^https?://scribd\\.com/.+",
       endpoint_url="http://www.scribd.com/services/oembed",
       title="Scribd"
     },

     #oembed_provider{
       url_re="^https?://viddler\\.com/explore/.*/videos/\\w+/?",
       endpoint_url="http://lab.viddler.com/services/oembed/",
       title="Viddler Video"
     },

     #oembed_provider{
       url_re="^https?://vimeo\\.com/.*",
       endpoint_url="http://www.vimeo.com/api/oembed.json",
       title="Vimeo"
     },

     #oembed_provider{
       url_re="^https?://((www\\.)?youtube\\.com/(watch|v/|embed/)|youto\\.be/\\w+)",
       endpoint_url="http://www.youtube.com/oembed",
       title="YouTube"
     },

     #oembed_provider{
       url_re="^https?://dotsub\\.com/view/[-\\da-zA-Z]+",
       endpoint_url="http://dotsub.com/services/oembed",
       title="dotSUB.com"
     },

     #oembed_provider{
       url_re="^https?://yfrog\\.(com|ru|com\\.tr|it|fr|co\\.il|co\\.uk|com\\.pl|pl|eu|us)/[a-zA-Z0-9]+",
       endpoint_url="http://www.yfrog.com/api/oembed",
       title="YFrog"
     },

     #oembed_provider{
       url_re="^https?://clikthrough\\.com/theater/video/\\d+",
       endpoint_url="http://clikthrough.com/services/oembed",
       title="Clikthrough"
     },

     #oembed_provider{
       url_re="^https?://kinomap\\.com/.+",
       endpoint_url="http://www.kinomap.com/oembed",
       title="Kinomap"
     },

     #oembed_provider{
       url_re="^https?://photobucket\\.com/(albums|groups)/.+",
       endpoint_url="http://photobucket.com/oembed",
       title="Photobucket"
     },

     #oembed_provider{
       url_re="^https?://www\\.slideshare\\.net/.+/.+",
       endpoint_url="http://www.slideshare.net/api/oembed/2",
       title="Slideshare"
     }

    ].








