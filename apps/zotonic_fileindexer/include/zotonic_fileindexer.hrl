%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2018 Marc Worrell
%% @doc Fileindex record used for directory indices.

%% Copyright 2018 Marc Worrell
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

-record(fileindex, {
    basename :: binary(),
    rootname :: binary(),
    extension :: binary(),
    relpath :: binary(),
    path :: binary()
}).
