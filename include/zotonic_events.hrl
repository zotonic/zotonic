%% @author Maas-Maarten Zeeman <mmzeeman@xs4all.nl>
%% @copyright 2012 Maas-Maarten Zeeman
%% @doc Events used in Zotonic core

%% Copyright 2012 Maas-Maarten Zeeman
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


%% @doc Postback event received from browser. 
%% 
-record(postback, {message, trigger, target}).

%% @doc Submit event received from browser. 
%%
-record(submit, {message, form, target}).


%% Drag and drop events. They are received in a normal postback event by scomp_base_droppable. 
%% The are emitted as separate event.
%%

%% @doc Drag event.
-record(drag, {drag, drop}).

%% @doc Drop event.
-record(drop, {drag, drop}).

%% Sort event. It is received in a normal postback event by scomp_base_sorter. 
%%

%% @doc Sort event.
-record(sort, {items, drop}).


