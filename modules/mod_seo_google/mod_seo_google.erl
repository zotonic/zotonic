%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% Date: 2009-08-16
%% @doc Search engine optimization for Google. Support for Google Analytics and Webmaster tools.

%% Copyright 2009 Marc Worrell
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

-module(mod_seo_google).
-author("Marc Worrell <marc@worrell.nl>").

-mod_title("SEO Google").
-mod_description("Support for Google Analytics and Webmaster tools.").
-mod_prio(600).
-mod_depends([seo]).
-mod_provides([seo_google]).
