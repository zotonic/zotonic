%% Licensed under the Apache License, Version 2.0 (the "License"); you may
%% not use this file except in compliance with the License. You may obtain
%% a copy of the License at <http://www.apache.org/licenses/LICENSE-2.0>
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% Alternatively, you may use this file under the terms of the GNU Lesser
%% General Public License (the "LGPL") as published by the Free Software
%% Foundation; either version 2.1, or (at your option) any later version.
%% If you wish to allow use of your version of this file only under the
%% terms of the LGPL, you should delete the provisions above and replace
%% them with the notice and other provisions required by the LGPL; see
%% <http://www.gnu.org/licenses/>. If you do not delete the provisions
%% above, a recipient may use your version of this file under the terms of
%% either the Apache License or the LGPL.
%%
%% @author Richard Carlsson <carlsson.richard@gmail.com>
%% @copyright 2006-2009 Richard Carlsson
%% @doc Erlang file monitoring service
%% 
%% The behaviour of this service is inspired by the open source FAM
%% daemon ([http://oss.sgi.com/projects/fam/]). It allows file system
%% paths to be monitored, so that a message will be sent to the client
%% process whenever a status change is detected. Currently, the only
%% supported method of detection is by regular polling by the server.
%% While it is not optimal, polling has less overhead than might be
%% expected, and is portable across platforms. The polling interval can
%% be adjusted; by default the server polls all monitored paths every 5
%% seconds. Recursive (automatic) monitoring is supported. The server
%% keeps track of its client processes, and removes all their monitors
%% if they should die.
%%
%% == Event messages ==
%%
%% When a new monitor is set up, or a change is detected, an event
%% message is sent to the client. These have the following general form:
%% <pre>{@type @{file_monitor, Ref::monitor(), Event@}}</pre>
%% where `Ref' is the monitor reference returned when the monitor was
%% set up, and `Event' is one of the following:
%% <ul>
%%  <li>{@type @{found, Path::binary(), Type, Info::#file_info@{@},
%%                      Entries::[{added|deleted, Name::binary()@}]@}}</li>
%%  <li>{@type @{changed, Path::binary(), Type, Info::#file_info@{@},
%%                      Entries::[{added|deleted, Name::binary()@}]@}}</li>
%%  <li>{@type @{error, Path::binary(), Type, PosixError::atom()@}}</li>
%% </ul>
%% where `Path' is the watched path (as a binary), `Type' is the type of
%% monitoring being performed (either `file' or `directory'), `Info' is
%% a `file_info' record as defined in `kernel/include/file.hrl', and
%% `Entries' is a list of tuples `{added, binary()}' and `{deleted,
%% binary()}' describing changes to the directory entries if `Type' is
%% `directory', otherwise this is always the empty list. For a `found'
%% event, all entries are `{added, Name}'.
%%
%% A `found' event is sent when a monitor is initially set up, if the
%% path can be read. After that, whenever a change in status is
%% detected, a `changed' event is sent. If the file does not exist or
%% could for some other reason not be accessed, an `error' event is sent
%% (both initially and for subsequent changes). In other words, the
%% first event for a path is always either `found' or `error', and later
%% events are either `changed' or `error'.
%% 
%% === Detection of file type changes ===
%%
%% If the object found at a path changes type in the interval between
%% two polls, for example if a directory is replaced by a file with the
%% same name, or vice versa, the file monitor server will detect this
%% and dispatch an `enoent' error event before the new status event. A
%% client can thus rely on always seeing the old file disappear before
%% any change that reports a different file type.
%% 
%% == Monitoring types ==
%%
%% There are two ways in which a path can be monitored: as a `file',
%% meaning that we are interested only in the object found at that path,
%% or as a `directory', meaning that we expect the path to point to a
%% directory, and we are also interested in the list of entries of that
%% directory.
%%
%% If a path is monitored as a directory, and the object at the path
%% exists but is not a directory, an `enotdir' error event will be
%% generated. An existing directory can however both be monitored as a
%% directory and as a file - the difference is that in the latter case,
%% the reported list of entries will always be empty.
%% 
%% == Automatic (recursive) monitoring ==
%%
%% Automatic monitoring (automonitoring for short) can be used to watch
%% a single file of any type, or a whole directory tree. The monitoring
%% type (`file' or `directory') used for any path is based on the actual
%% type of object found at the path (`directory' if the object is a
%% readable directory, and `file' otherwise). If the object is replaced
%% by another of different type, the monitoring type will change
%% automatically.
%%
%% When a directory becomes automonitored, all of its entries will also
%% be automatically monitored, recursively. As entries are created or
%% deleted in an automonitored directory, they will be dynamically added
%% or removed, respectively, from being monitored. The root path used to
%% create the automonitor will however always remain monitored (even if
%% the object temporarily or permanently disappears) until the server is
%% told to delete the monitor.
%%
%% The event messages sent to the client are the same as if manual
%% monitoring was done. A newly discovered path will be reported by a
%% `found' (or possibly, by an `error' event), and subsequent changes on
%% that path are reported by `changed' and `error' events. If the
%% monitoring type is changed, a new `found' event is sent, and so on.

-module(z_filewatcher_monitor).

-behaviour(gen_server).

-export([monitor_file/1, monitor_file/2, monitor_file/3, monitor_dir/1,
	 monitor_dir/2, monitor_dir/3, automonitor/1, automonitor/2,
	 automonitor/3, demonitor/1, demonitor/2, demonitor_file/2,
	 demonitor_file/3, demonitor_dir/2, demonitor_dir/3,
	 get_interval/0, get_interval/1, set_interval/1, set_interval/2,
	 normalize_path/1]).

-export([start/0, start/1, start/2, start_link/0, start_link/1,
	 start_link/2, stop/0, stop/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 code_change/3, terminate/2]).

-compile({no_auto_import,[monitor/2]}).
-compile({no_auto_import,[demonitor/2]}).

-include_lib("kernel/include/file.hrl").


%% NOTE: Monitored paths should be absolute, but this is not checked.
%% 
%% We never rewrite the paths, e.g. from relative to absolute, but we
%% convert every path into a binary internally, for the sake of
%% comparisons, and return it to the caller for reference.
%%
%% @type filename() = binary() | atom() | [char() | filename()]. This is
%% an "extended IO-list", that allows atoms as well as binaries to occur
%% either on their own or embedded in a list or deep list. The intent of
%% this is to accept any file name that can be used by the standard
%% library module `file', as well as any normal IO-list, and any list
%% that is formed by combining such fragments.
%%
%% @type options() = [term()]. A list of options.
%%
%% @type server_ref() = pid() | atom() | {Node::atom(), atom()} |
%% {global, atom()}. A reference to a running server. See {@link
%% //stdlib/gen_server:call/3} for more information.

-define(DEFAULT_INTERVAL, 5000).  % change with option interval
-define(MIN_INTERVAL, 100).
-define(TIME_TO_STABLE, 1100).  % (timestamps have second resolution)
-define(SERVER, ?MODULE).
-define(MSGTAG, ?SERVER).

%% % @type object() = {file|directory, filename()}
%% @type monitor() = reference(). A monitor reference.

-record(state, {poll=true,  % boolean(), false if polling is disabled
		interval,   % polling interval (milliseconds)
		files,      % map: file path -> #entry{}
		dirs,       % map: directory path -> #entry{}
		autodirs,   % map: directory path -> monitor() -> entries
		refs,       % map: monitor() -> #monitor_info{}
		clients     % map: client Pid -> #client_info{}
	       }).

-record(entry, {info = undefined,      % #file_info{} or posix atom
		dir = [],              % directory entries (if any)
		stable = 0,            % integer(), millis until dir stable
		monitors = sets:new()  % set(monitor())
	       }).

-record(client_info, {monitor,    % erlang:monitor/2 reference
		      refs        % set(monitor()); monitors owned by client
		     }).

-record(monitor_info, {pid,     % client Pid
		       auto,    % boolean(), true for an automonitor
		       objects  % set(object())
		      }).

%%
%% User interface
%%

%% @spec (filename()) ->
%%         {ok, monitor(), binary()} | {error, not_owner | automonitor}
%% @equiv monitor_file(Path, [])
monitor_file(Path) ->
    monitor_file(Path, []).

%% @spec (filename(), options()) ->
%%         {ok, monitor(), binary()} | {error, not_owner | automonitor}
%% @equiv monitor_file(file_monitor, Path, Opts)
monitor_file(Path, Opts) ->
    monitor_file(?SERVER, Path, Opts).

%% @spec (server_ref(), filename(), options()) ->
%%         {ok, monitor(), binary()} | {error, not_owner | automonitor}
%% @doc Monitors the specified file path. Returns the monitor reference
%% as well as the monitored path as a binary.
%%
%% Options:
%% <ul>
%%   <li>{@type @{monitor, monitor()@}}: specifies a reference for
%%   identifying the monitor to which the path should be added. The
%%   monitor need not already exist, but if it does, only the same
%%   process is allowed to add paths to it, and paths may not be added
%%   manually to an automonitor.</li>
%% </ul>
monitor_file(Server, Path, Opts) ->
    monitor(Server, Path, Opts, file).

%% @spec (filename()) ->
%%         {ok, monitor(), binary()} | {error, not_owner | automonitor}
%% @equiv monitor_dir(Path, [])
monitor_dir(Path) ->
    monitor_dir(Path, []).

%% @spec (filename(), options()) ->
%%         {ok, monitor(), binary()} | {error, not_owner | automonitor}
%% @equiv monitor_dir(file_monitor, Path, Opts)
monitor_dir(Path, Opts) ->
    monitor_dir(?SERVER, Path, Opts).

%% @spec (server_ref(), filename(), options()) ->
%%         {ok, monitor(), binary()} | {error, not_owner | automonitor}
%% @doc Monitors the specified directory path. Returns the monitor
%% reference as well as the monitored path as a binary.
%%
%% Options: see {@link monitor_file/3}.
monitor_dir(Server, Path, Opts) ->
    monitor(Server, Path, Opts, directory).


%% not exported
monitor(Server, Path, Opts, Type) ->
    FlatPath = normalize_path(Path),
    Ref = case proplists:get_value(monitor, Opts) of
	      R when is_reference(R) ; R =:= undefined -> R;
	      _ -> erlang:error(badarg)
	  end,
    Cmd = {monitor, self(), {Type, FlatPath}, Ref},
    case gen_server:call(Server, Cmd) of
	{ok, Ref1} -> {ok, Ref1, FlatPath};
	{error, Reason} -> {error, Reason}
    end.


%% @spec (filename()) -> {ok, monitor(), binary()}
%% @equiv automonitor(Path, [])
automonitor(Path) ->
    automonitor(Path, []).

%% @spec (filename(), options()) -> {ok, monitor(), binary()}
%% @equiv automonitor(file_monitor, Path, Opts)
automonitor(Path, Opts) ->
    automonitor(?SERVER, Path, Opts).

%% @spec (server_ref(), filename(), options()) -> {ok, monitor(), binary()}
%% @doc Automonitors the specified path. Returns the monitor reference as
%% well as the monitored path as a binary.
%%
%% Options: none at present.
automonitor(Server, Path, _Opts) ->
    FlatPath = normalize_path(Path),
    {ok, Ref} = gen_server:call(Server, {automonitor, self(), FlatPath}),
    {ok, Ref, FlatPath}.


%% @spec (monitor()) -> ok | {error, not_owner}
%% @equiv demonitor(file_monitor, Ref)
demonitor(Ref) ->
    demonitor(?SERVER, Ref).

%% @spec (server_ref(), monitor()) -> ok | {error, not_owner}
%% @doc Deletes the specified monitor. This can only be done by the
%% process that created the monitor.
demonitor(Server, Ref) when is_reference(Ref) ->
    ok = gen_server:call(Server, {demonitor, self(), Ref}).

%% @spec (filename(), monitor()) -> ok | {error, not_owner}
%% @equiv demonitor_file(file_monitor, Path, Ref)
demonitor_file(Path, Ref) ->
    demonitor_file(?SERVER, Path, Ref).

%% @spec (server_ref(), filename(), monitor()) -> ok | {error, not_owner}
%% @doc Removes the file path from the specified monitor. This can only
%% be done by the process that created the monitor.
demonitor_file(Server, Path, Ref) ->
    demonitor(Server, Path, Ref, file).

%% @spec (filename(), monitor()) -> ok | {error, not_owner}
%% @equiv demonitor_dir(file_monitor, Path, Ref)
demonitor_dir(Path, Ref) ->
    demonitor_dir(?SERVER, Path, Ref).

%% @spec (server_ref(), filename(), monitor()) -> ok | {error, not_owner}
%% @doc Removes the directory path from the specified monitor. This can
%% only be done by the process that created the monitor.
demonitor_dir(Server, Path, Ref) ->
    demonitor(Server, Path, Ref, directory).

%% not exported
demonitor(Server, Path, Ref, Type) when is_reference(Ref) ->
    FlatPath = normalize_path(Path),
    ok = gen_server:call(Server, {demonitor, self(), {Type, FlatPath}, Ref}).


%% @spec () -> integer()
%% @equiv get_interval(file_monitor)
get_interval() ->
    get_interval(?SERVER).

%% @spec (server_ref()) -> integer()
%% @doc Returns the current polling interval.
get_interval(Server) ->
    gen_server:call(Server, get_interval).


%% @spec (integer()) -> ok
%% @equiv set_interval(file_monitor, Time)
set_interval(Time) ->
    set_interval(?SERVER, Time).

%% @spec (server_ref(), integer()) -> ok
%% @doc Sets the polling interval. Units are in milliseconds.
set_interval(Server, Time) when is_integer(Time) ->
    gen_server:call(Server, {set_interval, Time}).


%% @spec () -> {ok, ServerPid::pid()} | ignore | {error, any()}
%% @equiv start([])
start() ->
    start([]).

%% @spec (options()) -> {ok, ServerPid::pid()} | ignore | {error, any()}
%% @equiv start({local, file_monitor}, Options)
start(Options) ->
    start({local, ?SERVER}, Options).

%% @spec ({local, atom()} | {global, atom()} | undefined, options()) ->
%%         {ok, ServerPid::pid()} | ignore | {error, any()}
%% @doc Starts the server and registers it using the specified name.
%% If the name is `undefined', the server will not be registered. See
%% {@link //stdlib/gen_server:start_link/4} for details about the return
%% value.
%%
%% Options:
%% <ul>
%%   <li>{@type {interval, Milliseconds::integer()@}}</li>
%% </ul>
start(undefined, Options) ->
    gen_server:start(?MODULE, Options, []);
start(Name, Options) ->
    gen_server:start(Name, ?MODULE, Options, []).

%% @spec () -> {ok, ServerPid::pid()} | ignore | {error, any()}
%% @equiv start_link([])
start_link() ->
    start_link([]).

%% @spec (options()) -> {ok, ServerPid::pid()} | ignore | {error, any()}
%% @equiv start_link({local, file_monitor}, Options)
start_link(Options) ->
    start_link({local, ?SERVER}, Options).

%% @spec ({local, atom()} | {global, atom()} | undefined, options()) ->
%%         {ok, ServerPid::pid()} | ignore | {error, any()}
%% @doc Starts the server, links it to the current process, and
%% registers it using the specified name. If the name is `undefined',
%% the server will not be registered. See {@link
%% //stdlib/gen_server:start_link/4} for details about the return value.
%%
%% Options: see {@link start/2}.
start_link(undefined, Options) ->
    gen_server:start_link(?MODULE, Options, []);
start_link(Name, Options) ->
    gen_server:start_link(Name, ?MODULE, Options, []).


%% @spec () -> ok
%% @equiv stop(file_monitor)
stop() ->
    stop(?SERVER).

%% @spec (server_ref()) -> ok
%% @doc Stops the specified server.
stop(Server) ->
    gen_server:call(Server, stop),
    ok.


%%
%% gen_server callbacks
%%

%% @private
init(Options) ->
    Time = safe_interval(proplists:get_value(interval, Options)),
    St = #state{interval = Time,
		files = dict:new(),
		dirs = dict:new(),
		autodirs = dict:new(),
		clients = dict:new(),
		refs = dict:new()},
    set_timer(St),
    {ok, St}.

%% Note that we create all references on the server side, to be
%% consistent (and in case it will matter, to ensure that the server
%% mostly uses references local to its own node).

%% @private
handle_call({monitor, Pid, Object, undefined}, From, St) ->
    handle_call({monitor, Pid, Object, make_ref()}, From, St);
handle_call({monitor, Pid, Object, Ref}, _From, St)
  when is_pid(Pid), is_reference(Ref) ->
    try add_monitor(Object, Pid, Ref, St) of
	St1 -> {reply, {ok, Ref}, register_client(Pid, Ref, St1)}
    catch
	not_owner ->
	    {reply, {error, not_owner}, St};
	automonitor ->
	    {reply, {error, automonitor}, St}
    end;
handle_call({demonitor, Pid, Ref}, _From, St) when is_reference(Ref) ->
    try delete_monitor(Pid, Ref, St) of
	St1 -> {reply, ok, St1}
    catch
	not_owner ->
	    {reply, {error, not_owner}, St}
    end;
handle_call({demonitor, Pid, Object, Ref}, _From, St) when is_reference(Ref) ->
    try demonitor_path(Pid, Ref, Object, St) of
	St1 -> {reply, ok, St1}
    catch
	not_owner ->
	    {reply, {error, not_owner}, St}
    end;
handle_call({automonitor, Pid, Path}, _From, St) when is_pid(Pid) ->
    %% it shouldn't be possible to get exceptions due to wrong owner or
    %% non-automonitor type here, since we always create a new reference
    Ref = make_ref(),
    St1 = unsafe_automonitor_path(Path, Pid, Ref, St),
    {reply, {ok, Ref}, register_client(Pid, Ref, St1)};
handle_call(get_interval, _From, St) ->
    {reply, St#state.interval, St};
handle_call({set_interval, Time}, _From, St) ->
    {reply, ok, St#state{interval=safe_interval(Time)}};
handle_call(stop, _From, St) ->
    {stop, normal, ok, St}.

%% @private
handle_cast(_, St) ->
    {noreply, St}.

%% @private
handle_info({?MSGTAG, Ref, Event}, St) ->
    %% auto-monitoring event to self
    case dict:find(Ref, St#state.refs) of
	{ok, #monitor_info{pid=Pid}} ->
	    {noreply, autoevent(Event, Pid, Ref, St)};
	error ->
	    %% could happen if this event was already in the queue when
	    %% we processed the deletion of the same reference, so just
	    %% ignore the message
	    {noreply, St}
    end;
handle_info(poll, St) ->
    {noreply, set_timer(poll(St))};
handle_info(enable_poll, St) ->
    {noreply, St#state{poll=true}};
handle_info({'DOWN', _Ref, process, Pid, _Info}, St) ->
    {noreply, remove_client(Pid, St)};
handle_info(_, St) ->
    {noreply, St}.

%% @private
code_change(_OldVsn, St, _Extra) ->
    {ok, St}.

%% @private
terminate(_Reason, _St) ->
    ok.

%%
%% Internal functions
%%

%% We allow paths as binaries, atoms, or "extended io-lists" that may
%% contain atoms as well as binaries. This is flattened into a single
%% binary (currently assuming that the input uses an 8-bit encoding).
%% A single character is not a valid path; it must be within a list.

%% @spec (filename()) -> binary()
%% @doc Flattens the given path to a single binary.

normalize_path(Path) when is_binary(Path) -> Path;
normalize_path(Path) ->
    list_to_binary(flatten_onto(Path, [])).

flatten_onto([X | Xs], As) when is_integer(X), X >= 0, X =< 255 ->
    [X | flatten_onto(Xs, As)];
flatten_onto([X | Xs], As) ->
    flatten_onto(X, flatten_onto(Xs, As));
flatten_onto([], As) ->
    As;
flatten_onto(X, As) when is_atom(X) ->
    atom_to_list(X) ++ As;
flatten_onto(X, As) when is_binary(X) ->
    binary_to_list(X) ++ As;
flatten_onto(_, _) ->
    erlang:error(badarg).

join_to_path(Path, File) when is_binary(Path), is_binary(File) ->
    normalize_path(filename:join(binary_to_list(Path),
				 binary_to_list(File))).

safe_interval(N) when is_integer(N) ->
    min(16#FFFFffff, max(N, ?MIN_INTERVAL));
safe_interval(_) -> ?DEFAULT_INTERVAL.

set_timer(St) ->
    erlang:send_after(St#state.interval, self(), poll),
    St.

%% Handling of auto-monitoring events
%%
%% - If a new entry of an automonitored directory is discovered, make it
%%   too automonitored (this does recursive monitoring by definition).
%%
%% - If an entry is deleted from an automonitored directory, remove any
%%   automonitor from it and its subdirectories recursively. Note that
%%   by definition, this will never auto-remove the top automonitored
%%   directory.
%%
%% - Because of the special status of the top directory (the path given
%%   to automonitor), we do not allow the user to add/remove paths to an
%%   existing automonitor or pass a user-specified reference.
%%
%% - Note that to be able to demonitor directories recursively, we must
%%   track the automonitored directory entries for each directory and
%%   monitor pair (since the directory itself can no longer be read,
%%   there is no other way we can know which subentries to demonitor).
%%
%% - An automonitored non-directory that changes type to directory, or
%%   vice versa, should should cause recreation of the monitor to match
%%   the new type.
%%
%% - Errors on automonitored files are assumed to be intermittent, i.e.,
%%   not even enoent should in itself cause demonitoring - that is done
%%   only if the containing directory reports that the file is removed.
%%
%% - Errors on automonitored directories cause immediate demonitoring of
%%   the entries of the directory, but not the directory itself.

autoevent({_Tag, Path, Type, #file_info{}=Info, _Files}, Pid, Ref, St)
  when (((Type =:= file) and (Info#file_info.type =:= directory)) orelse
	((Type =:= directory) and (Info#file_info.type =/= directory))) ->
    %% monitor type mismatch detected
    autoremonitor_path(Path, Pid, Ref, St);
autoevent({_Tag, Path, directory, #file_info{}=Info, Files}, Pid, Ref, St0)
  when Info#file_info.type =:= directory ->
    %% add/remove automonitoring to/from all added/deleted entries
    lists:foldl(fun ({added, File}, St) ->
			St1 = add_autodir_entry(Path, File, Ref, St),
			automonitor_path(join_to_path(Path, File),
					 Pid, Ref, St1);
		    ({deleted, File}, St) ->
			St1 = remove_autodir_entry(Path, File, Ref, St),
			autodemonitor_path(join_to_path(Path, File),
					   Pid, Ref, St1)
		end,
		St0, Files);
autoevent({error, Path, directory, enotdir}, Pid, Ref, St) ->
    %% monitor type mismatch detected
    autoremonitor_path(Path, Pid, Ref, St);
autoevent({error, Path, directory, _}, Pid, Ref, St) ->
    %% only demonitor subdirectories/files
    autodemonitor_dir_entries(Path, Pid, Ref, St);
autoevent(_Event, _Pid, _Ref, St) ->
    St.

%% monitor type mismatch detected - recreate it to get correct type
autoremonitor_path(Path, Pid, Ref, St) ->
    automonitor_path(Path, Pid, Ref, autodemonitor_path(Path, Pid, Ref, St)).

automonitor_path(Path, Pid, Ref, St) ->
    %% Pid should be a known client, otherwise do nothing
    case dict:is_key(Pid, St#state.clients) of
	true ->
	    try unsafe_automonitor_path(Path, Pid, Ref, St)
	    catch
		throw:_ -> St
	    end;
	false ->
	    St
    end.

%% see add_monitor for possible thrown exceptions
unsafe_automonitor_path(Path, Pid, Ref, St) ->
    Object = case file:read_file_info(binary_to_list(Path)) of
		 {ok, #file_info{type=directory}} ->
		     {directory, Path};
		 _ ->
		     {file, Path}    % also for errors
	     end,
    add_automonitor(Object, Pid, Ref, St).

autodemonitor_path(Path, Pid, Ref, St0) ->
    St1 = try demonitor_path(Pid, Ref, {file, Path}, St0) 
 	  catch
 	      not_owner -> St0
 	  end,
    St2 = try demonitor_path(Pid, Ref, {directory, Path}, St1)
	  catch
	      not_owner -> St1
	  end,
    autodemonitor_dir_entries(Path, Pid, Ref, St2).

autodemonitor_dir_entries(Path, Pid, Ref, St0) ->
    Dirs0 = St0#state.autodirs,
    case dict:find(Path, Dirs0) of
	{ok, Map0} ->
	    case dict:find(Ref, Map0) of
		{ok, Set} ->
		    Map = dict:erase(Ref, Map0),
		    %% purge empty entries to save space
		    Dirs = case dict:size(Map) > 0 of
			       true -> dict:store(Path, Map, Dirs0);
			       false -> dict:erase(Path, Dirs0)
			   end,
		    St1 = St0#state{autodirs = Dirs},
		    sets:fold(fun (File, St) ->
				      P = join_to_path(Path, File),
				      autodemonitor_path(P, Pid, Ref, St)
			      end,
			      St1, Set);
		error ->
		    St0
	    end;
	error ->
	    St0
    end.

%% tracking subentries of automonitored directories, in order to enable
%% efficient recursive demonitoring

add_autodir_entry(Path, File, Ref, St) ->
    Map = case dict:find(Path, St#state.autodirs) of
	      {ok, Map0} ->
		  Set = case dict:find(Ref, Map0) of
			    {ok, Entries} -> Entries;
			    error -> sets:new()
			end,
		  dict:store(Ref, sets:add_element(File, Set), Map0);
	      error ->
		  dict:store(Ref, sets:add_element(File, sets:new()),
			     dict:new())
	  end,
    St#state{autodirs = dict:store(Path, Map, St#state.autodirs)}.

remove_autodir_entry(Path, File, Ref, St) ->
    Dirs0 = St#state.autodirs,
    case dict:find(Path, Dirs0) of
	{ok, Map0} ->
	    case dict:find(Ref, Map0) of
		{ok, Set0} ->
		    %% purge empty entries to save space
		    Set = sets:del_element(File, Set0),
		    Map = case sets:size(Set) > 0 of
			      true -> dict:store(Ref, Set, Map0);
			      false -> dict:erase(Ref, Map0)
			  end,
		    Dirs = case dict:size(Map) > 0 of
			       true -> dict:store(Path, Map, Dirs0);
			       false -> dict:erase(Path, Dirs0)
			   end,
		    St#state{autodirs = Dirs};
		error -> St
	    end;
	error ->
	    St
    end.

%% client monitoring (once a client, always a client - until death)

register_client(Pid, Ref, St) ->
    Info = case dict:find(Pid, St#state.clients) of
	       {ok, OldInfo} -> OldInfo;
	       error ->
		   Monitor = erlang:monitor(process, Pid),
		   #client_info{monitor = Monitor, refs = sets:new()}
	   end,
    Refs = sets:add_element(Ref, Info#client_info.refs),
    St#state{clients = dict:store(Pid, Info#client_info{refs = Refs},
				  St#state.clients)}.

remove_client(Pid, St) ->
    case dict:find(Pid, St#state.clients) of
	{ok, #client_info{monitor = Monitor, refs = Refs}} ->
	    erlang:demonitor(Monitor, [flush]),
	    purge_client(Pid, Refs, St);
	error ->
	    St
    end.

purge_client(Pid, Refs, St0) ->
    sets:fold(fun (Ref, St) ->
		      %% the Pid *should* be the owner here, so
		      %% a not_owner exception should not happen
		      delete_monitor(Pid, Ref, St)
	      end,
	      St0#state{clients = dict:erase(Pid, St0#state.clients)},
	      Refs).

%% Adding a new monitor; throws 'not_owner' if the monitor reference is
%% already registered for another Pid; throws 'automonitor' if the
%% reference is already registered with a different type.

add_monitor(Object, Pid, Ref, St) ->
    add_monitor(Object, Pid, Ref, St, false).

add_automonitor(Object, Pid, Ref, St) ->
    add_monitor(Object, Pid, Ref, St, true).

add_monitor(Object, Pid, Ref, St, Auto) ->
    Info = case dict:find(Ref, St#state.refs) of
	       {ok, #monitor_info{pid = Pid, auto = Auto}=OldInfo} ->
		   OldInfo;
	       {ok, #monitor_info{pid = Pid}} ->
		   throw(automonitor);
	       {ok, #monitor_info{}} ->
		   throw(not_owner);
	       error ->
		   #monitor_info{pid = Pid, auto = Auto,
				 objects = sets:new()}
	   end,
    NewObjects = sets:add_element(Object, Info#monitor_info.objects),
    Refs = dict:store(Ref, Info#monitor_info{objects = NewObjects},
		      St#state.refs),
    monitor_path(Object, Ref, St#state{refs = Refs}).

%% We must separate the namespaces for files and dirs; there may be
%% simultaneous file and directory monitors for the same path, and a
%% file may be deleted and replaced by a directory of the same name, or
%% vice versa. The client should know (more or less) if a path is
%% expected to refer to a file or a directory.

monitor_path({file, Path}, Ref, St) ->
    St#state{files = monitor_path(Path, Ref, file, St#state.files, St)};
monitor_path({directory, Path}, Ref, St) ->
    St#state{dirs = monitor_path(Path, Ref, directory, St#state.dirs, St)}.

%% Adding a new monitor forces an immediate poll of the path, such that
%% previous monitors only see any real change, while the new monitor
%% either gets {found, ...} or {error, ...}.

monitor_path(Path, Ref, Type, Dict, St) ->
    Entry = case dict:find(Path, Dict) of
		{ok, OldEntry} -> poll_file(Path, OldEntry, Type, St);
		error -> new_entry(Path, Type, St)
	    end,
    event(#entry{}, dummy_entry(Entry, Ref), Type, Path, St),
    NewEntry = Entry#entry{monitors =
			   sets:add_element(Ref, Entry#entry.monitors)},
    dict:store(Path, NewEntry, Dict).

dummy_entry(Entry, Ref) ->
    Entry#entry{monitors = sets:add_element(Ref, sets:new())}.

new_entry(Path, Type, St) ->
    refresh_entry(Path, #entry{monitors = sets:new()}, Type,
		  St#state.interval).

%% Deleting a monitor by reference; throws not_owner if the monitor
%% reference is owned by another Pid. The client_info entry may already
%% have been deleted if we come from purge_client().

delete_monitor(Pid, Ref, St0) ->
    St1 = case dict:find(Pid, St0#state.clients) of
	      {ok, #client_info{refs = Refs}=I} ->
		  NewRefs = sets:del_element(Ref, Refs),
		  St0#state{clients =
			    dict:store(Pid, I#client_info{refs = NewRefs},
				       St0#state.clients)};
	      error ->
		  St0
	  end,
    case dict:find(Ref, St1#state.refs) of
	{ok, #monitor_info{pid = Pid, objects = Objects}} ->
	    sets:fold(fun (Object, St) ->
			      purge_monitor_path(Ref, Object, St)
		      end,
		      St1#state{refs = dict:erase(Ref, St1#state.refs)},
		      Objects);
	{ok, #monitor_info{}} -> throw(not_owner);
	error ->
	    St1
    end.

%% Deleting a particular path from a monitor. Throws not_owner if the
%% monitor reference is owned by another Pid.

demonitor_path(Pid, Ref, Object, St) ->
    case dict:find(Ref, St#state.refs) of
	{ok, #monitor_info{pid = Pid, objects = Objects}=I} ->
	    St1 = purge_monitor_path(Ref, Object, St),
	    I1 = I#monitor_info{objects=sets:del_element(Object, Objects)},
	    St1#state{refs = dict:store(Ref, I1, St1#state.refs)};
	{ok, #monitor_info{}} -> throw(not_owner);
	error ->
	    St
    end.

%% Deleting a particular monitor from a path.

purge_monitor_path(Ref, {file, Path}, St) ->
    St#state{files = purge_monitor_path_1(Path, Ref, St#state.files)};
purge_monitor_path(Ref, {directory, Path}, St) ->
    St#state{dirs = purge_monitor_path_1(Path, Ref, St#state.dirs)}.

purge_monitor_path_1(Path, Ref, Dict) ->
    case dict:find(Path, Dict) of
	{ok, Entry} -> 
	    Monitors = sets:del_element(Ref, Entry#entry.monitors),
	    case sets:size(Monitors) > 0 of
		true ->
		    dict:store(Path, Entry#entry{monitors = Monitors}, Dict);
		false ->
		    dict:erase(Path, Dict)
	    end;
	error ->
	    Dict
    end.

%% Generating events upon state changes by comparing old and new states
%% 
%% Event formats:
%%   {found, Path, Type, #file_info{}, Files}
%%   {changed, Path, Type, #file_info{}, Files}
%%   {error, Path, Type, PosixAtom}
%%
%% Type is file or directory, as specified by the monitor type, not by
%% the actual type on disk. If Type is file, Files is always []. If Type
%% is directory, Files is a list of {added, FileName} and {deleted,
%% FileName}, where FileName is on basename form, i.e., without any
%% directory component.
%%
%% When a new monitor is installed for a path, an initial {found,...}
%% or {error,...} event will be sent to the monitor owner.
%%
%% Subsequent events will be either {changed,...} or {error,...}.
%%
%% The monitor reference is not included in the event descriptor itself,
%% but is part of the main message format; see cast/2.
%%
%% Note that we never compare directory entry lists here; if there might
%% have been changes, the timestamps should also be different - see
%% refresh_entry() below for more details.

event(#entry{info = Info, stable = Stable},
      #entry{info = Info, stable = Stable}, _Type, _Path, _St) ->
    ok;    % no change in state 
event(#entry{info = undefined}, #entry{info = NewInfo}=Entry,
      Type, Path, St)
  when not is_atom(NewInfo) ->
    %% file or directory exists, for a fresh monitor
    Diff = diff_lists(Entry#entry.dir, []),
    cast({found, Path, Type, NewInfo, Diff}, Entry#entry.monitors, St);
event(OldEntry, #entry{info = NewInfo}=Entry, Type, Path, St)
  when is_atom(NewInfo) ->
    %% file or directory is not available
    type_change_event(OldEntry#entry.info, NewInfo, Type, Path, Entry, St),
    cast({error, Path, Type, NewInfo}, Entry#entry.monitors, St);
event(OldEntry, #entry{info = NewInfo}=Entry, Type, Path, St) ->
    %% a file or directory has changed or become readable again after an error
    type_change_event(OldEntry#entry.info, NewInfo, Type, Path, Entry, St),
    Diff = diff_lists(Entry#entry.dir, OldEntry#entry.dir),
    if Diff =:= [],
       Entry#entry.stable =/= OldEntry#entry.stable,
       NewInfo =:= OldEntry#entry.info ->
	    %% if only the time-to-stable has changed, we must not
	    %% broadcast an event unless there really was a change in
	    %% the directory list
	    ok;
       true ->
	    cast({changed, Path, Type, NewInfo, Diff},
		 Entry#entry.monitors, St)
    end.

%% sudden changes in file type cause an 'enoent' error report before the
%% new status, so that clients do not need to detect this themselves
type_change_event(#file_info{type = T}, #file_info{type = T}, _, _, _, _) ->
    ok;
type_change_event(#file_info{}, #file_info{}, Type, Path, Entry, St) ->
    cast_enoent(Type, Path, Entry, St);
type_change_event(#file_info{type = directory}, enotdir, Type, Path, Entry,
		  St) ->
    cast_enoent(Type, Path, Entry, St);
type_change_event(enotdir, #file_info{type = directory}, Type, Path, Entry,
		  St) ->
    cast_enoent(Type, Path, Entry, St);
type_change_event(_, _, _, _, _, _) ->
    ok.

cast_enoent(Type, Path, Entry, St) ->
    cast({error, Path, Type, enoent}, Entry#entry.monitors, St).

poll(#state{poll=false}=St) ->
    St;
poll(#state{poll=true}=St) ->
    Files = dict:map(fun (Path, Entry) ->
			     poll_file(Path, Entry, file, St)
		     end,
		     St#state.files),
    Dirs = dict:map(fun (Path, Entry) ->
			    poll_file(Path, Entry, directory, St)
		    end,
		    St#state.dirs),
    %% polling will now be disabled until all automonitoring events have
    %% been processed:
    self() ! enable_poll,
    St#state{poll=false, files = Files, dirs = Dirs}.

poll_file(Path, Entry, Type, St) ->
    NewEntry = refresh_entry(Path, Entry, Type, St#state.interval),
    event(Entry, NewEntry, Type, Path, St),
    NewEntry.

%% We want to minimize the polling cost, so we only list directories
%% when they have new timestamps, or are still considered "unstable".
%% This requires some explanation: Recall that timestamps have
%% whole-second resolution. If we listed a directory during the same
%% second that it was (or is still being) modified, it is possible that
%% we did not see all the additions/deletions that were made that second
%% - and in that case, we might not detect those changes until the
%% timestamp changes again (maybe never). (This really happens - in
%% particular in test suites...) But we cannot use the system clock for
%% comparisons, because the file system might be on another clock (or
%% another time zone setting), and read-access timestamps are also not
%% reliable (not consistently updated on all platforms and not at all on
%% some file systems). Therefore we do our own internal approximation of
%% passed time (#state.time), based on the refresh cycle.
%% Furthermore, we should make sure that the timestamps we use are
%% always from after the directory was last listed, so that we never
%% report an added entry that was actually created later than the
%% directory timestamp.

refresh_entry(Path, Entry, Type, Delta) when is_binary(Path) ->
    refresh_entry_0(binary_to_list(Path), Entry, Type, Delta,
		    Entry#entry.info).

refresh_entry_0(Path, Entry, Type, Delta, OldInfo) ->
    refresh_entry_1(Path, Entry, Type, Delta, OldInfo, false).

refresh_entry_1(Path, Entry, Type, Delta, OldInfo, Break) ->
    NewInfo = get_file_info(Path),
    case Type of
	directory when not is_atom(NewInfo) ->
	    case NewInfo#file_info.type of
		directory when is_atom(OldInfo)
		; NewInfo#file_info.mtime =/= OldInfo#file_info.mtime ->
		    %% The info has changed, so we are forced to refresh
		    %% the directory listing. We set the time-to-stable,
		    %% and loop to ensure that the info is always later
		    %% than the listing. See below for more details.
		    refresh_entry_2(Path, Entry, Type, Delta, NewInfo,
				    ?TIME_TO_STABLE);
		directory when Entry#entry.stable > 0, not Break ->
		    %% If both the old and new timestamps exist and the
		    %% modification time has not changed, but the entry
		    %% is not yet stable (and we didn't loop just now),
		    %% we need to refresh the directory list again and
		    %% decrement the time-to-stable. Note that we test
		    %% *before* we decrement, which ensures that this
		    %% happens at least once after the initial listing
		    %% regardless of the value of Delta. (See the
		    %% discussion about timestamp resolution above.)
		    refresh_entry_2(Path, Entry, Type, Delta, NewInfo,
				    max(0, Entry#entry.stable - Delta));
		directory ->
		    Entry#entry{info = NewInfo};
		_ ->
		    %% attempting to monitor a non-directory as a
		    %% directory is reported as an 'enotdir' error
		    Entry#entry{info = enotdir, dir = [], stable = 0}
	    end;
	_ ->
	    %% If we're not monitoring this path as a directory, or we
	    %% got an error, we don't care what kind of object it is,
	    %% but just track its status. To handle the case of an error
	    %% on a directory type monitor, we make sure to reset the
	    %% list of directory entries.
	    Entry#entry{info = NewInfo, dir = [], stable = 0}
    end.

refresh_entry_2(Path, Entry, Type, Delta, Info, Stable) ->
    %% refresh directory list, update time-to-stable, and loop to
    %% re-read the info, but also ensure that we do not trigger the
    %% stability rule again until the next poll
    refresh_entry_1(Path,
		    Entry#entry{info = Info,
				dir = list_dir(Path),
				stable = Stable},
		    Type, Delta, Info, true).

%% We clear some fields of the file_info so that we only trigger on real
%% changes; see the //kernel/file.erl manual and file.hrl for details.

get_file_info(Path) when is_list(Path) ->
    case file:read_file_info(Path) of
	{ok, Info} ->
	    Info#file_info{access = undefined,
			   atime  = undefined};
	{error, Error} ->
	    Error  % posix error code as atom
    end.

%% Listing the members of a directory; note that it yields the empty
%% list if it fails - this is not the place for error detection.

%% mw 20151023: patch to ignore Zotonic "files" directories containg all uploaded
%%              and preview files. This speeds up the scanning considerably.
list_dir(Path) when is_list(Path) ->
    Files = case file:list_dir(Path) of
		{ok, Fs} -> [normalize_path(F) || F <- Fs, F =/= <<"files">>, F =/= "files"];
		{error, _} -> []
	    end,
    lists:sort(Files).

%% both lists must be sorted for this diff to work

diff_lists([F1 | Fs1], [F2 | _]=Fs2) when F1 < F2 ->
    [{added, F1} | diff_lists(Fs1, Fs2)];
diff_lists([F1 | _]=Fs1, [F2 | Fs2]) when F1 > F2 ->
    [{deleted, F2} | diff_lists(Fs1, Fs2)];
diff_lists([_ | Fs1], [_ | Fs2]) ->
    diff_lists(Fs1, Fs2);
diff_lists([F | Fs1], Fs2) ->
    [{added, F} | diff_lists(Fs1, Fs2)];
diff_lists(Fs1, [F | Fs2]) ->
    [{deleted, F} | diff_lists(Fs1, Fs2)];
diff_lists([], []) ->
    [].

%% Multicasting events to clients. The message has the form
%% {file_monitor, MonitorReference, Event}, where Event is described in
%% more detail above, and 'file_monitor' is the name of this module.

cast(Message, Monitors, St) ->
    sets:fold(fun (Ref, Msg) ->
		      case dict:find(Ref, St#state.refs) of
			  {ok, #monitor_info{pid = Pid, auto = Auto}} ->
			      Pid ! {?MSGTAG, Ref, Msg},
			      case Auto of
				  true -> self() ! {?MSGTAG, Ref, Msg};
				  false -> ok
			      end
		      end,
		      Msg  % note that this is a fold, not a map
	      end,
	      Message, Monitors).