%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2011-2023 Marc Worrell
%% @doc Record definitions for wired events.

%% Copyright 2011-2023 Marc Worrell
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
-record(postback, {
    message :: term(),
    trigger :: binary() | undefined,
    target :: binary() | undefined
}).

%% @doc Submit event received from browser.
%%
-record(submit, {
    message :: term(),
    form :: binary() | undefined,
    target :: binary() | undefined
}).

%% The record of the postback_notify event is defined in zotonic_notifications.hrl

%% Drag and drop events. They are received in a normal postback event by scomp_base_droppable.
%% The are emitted as separate event.
%%

%% [Deprecated] Drag and drop event message -- used by scomps draggable and droppable
-record(dragdrop, {tag, delegate, id}).

%% @doc Drag event.
-record(drag, {drag, drop}).

%% @doc Drop event.
-record(drop, {drag, drop}).

%% Sort event. It is received in a normal postback event by scomp_base_sorter.
%%

%% @doc Sort event.
-record(sort, {items, drop}).

