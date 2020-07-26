%%%-----------------------------------------------------------------------------
%%% File:      zotonic_system.erl
%%% @author    Eric 'crownedgrouse' Pailleau <zotonic@crownedgrouse.com>
%%% @copyright 2020 Eric Pailleau
%%% @doc
%%% @end
%%%-----------------------------------------------------------------------------

-module(zotonic_system).
-author("Eric 'crownedgrouse' Pailleau <zotonic@crownedgrouse.com>").
-vsn("0.1.0").

-mode(compile).
-export([main/1]).

-include("zotonic_system.hrl").

%-include_lib("kernel/include/file.hrl").
-define(OptList,
        [
          {help,       $h,         "help",    undefined      , "This help"}
         ,{verbose,    $v,      "verbose",     string        , "Verbosity - [0..9] 0 mini, 9 maxi"}
         ,{version,    $V,      "version",    undefined      , "Version"}
         ,{list,       $L,      "list",       undefined      , "List all (category/app/target)"}
         ,{categories, $C,      "categories", undefined      , "List all categories"}
         ,{apps,       $A,      "apps",       undefined      , "List all applications for a category (require -c)"}
         ,{targets,    $T,      "targets",    undefined      , "List all targets for category and application (require -c, -a)."}
         ,{category,   $c,      "category",   string         , "Category"}
         ,{app,        $a,      "app",        string         , "Application"}
         ,{target,     $t,      "target",     string         , "Target (require -c, -a)"}
         ,{path,       $p,      "path",       string         , "Path to a category/app/target (see -L output)"}
        ]).

-define(Requires,
	    [
	       {apps,    [category]}
          ,{targets, [category, app]}
          ,{target,  [category, app]}
	    ]
	   ).
%%*** Main *********************************************************************
-spec main(list()) -> ok | no_return().

main([]) ->
    getopt:usage(?OptList, escript:script_name());
main(Args) ->
	 put(start_time, erlang:system_time(millisecond)),
	 put(stderr, standard_error),
	 put(stdout, standard_io),
    try
		% Raising exceptions commands (immediate exit command first)
        zotonic_system_cmd(Args),
        % Last resort command
        throw({help, ?MODULE})
    catch
		throw:Cmd -> 
			case Cmd of
	            {help, W} when (W =:= ?MODULE )  -> 
	            	getopt:usage(?OptList, escript:script_name()),
	                my_halt(1);
				{version, W} when (W =:= ?MODULE ) -> version() ;
				{?MODULE, Opt, _} -> 
					try 
						zotonic_system(Opt)
					catch
						_:R:Stacktrace -> % Erlang 21 or newer
							?PRINT(R,-1),
							?TRACE(Stacktrace),
	                        my_halt(2)
            after
                my_halt(0)
			end;
  				Z -> 
  					?ERROR(io_lib:format("Unexpected error. ~p~n",[Z])),
  					my_halt(3)
         end;
        _:Reason -> 
        	?ERROR(io_lib:format("Error. ~p~n",[Reason])), 
        	my_halt(4)
    end.

%%------------------------------------------------------------------------------
%% @doc Wrapper of halt 
%% @end
%%------------------------------------------------------------------------------
my_halt(Exit) ->
   % Write here any extra info if required
   halt(Exit).

%%------------------------------------------------------------------------------
%% @doc Global command line options testing
%% @end
%%------------------------------------------------------------------------------
zotonic_system_cmd(Args) -> ?TEST_HELP(?OptList, Args, zotonic_system).

%%------------------------------------------------------------------------------
%% @doc Check command line requirement depending options
%% @end
%%------------------------------------------------------------------------------
check_requirements(Opts) ->
   Fun = fun({K, L}) -> 
   			case lists:member(K, Opts) of  
   				false -> ok ;
   				true  -> % Check presence of mandatory other options 
   						Pred = fun(X) -> 
   									case (lists:member(X, Opts) or lists:keymember(X, 1, Opts)) of
   										true  -> true ;
   										false -> 
   											Ks = element(2, proplists:lookup(K, ?OptList)),
   											Kl = element(3, proplists:lookup(K, ?OptList)),
   											Rs = element(2, proplists:lookup(X, ?OptList)),
   											Rl = element(3, proplists:lookup(X, ?OptList)),
   											M = io_lib:format("Argument -~c/--~ts requires -~c/--~ts", [Ks, Kl, Rs, Rl]),
   											?ERROR(M), 
   											false
   									end
   						        end,
   				        case lists:all(Pred, L) of
   				        	true -> ok;
   				        	false -> halt(1)
   				        end
   			end 
   		end,
   lists:foreach(Fun, ?Requires).

%%******************************************************************************
%%*** Version ******************************************************************
%%******************************************************************************
%%------------------------------------------------------------------------------
%% @doc Print version
%% @end
%%------------------------------------------------------------------------------
-spec version() -> list().

version() -> 
	io:format("~ts~n",[get_version(?MODULE)]).

%%------------------------------------------------------------------------------
%% @doc Get version of a module
%% @end
%%------------------------------------------------------------------------------
-spec get_version(atom()) -> list().

get_version(Module) when is_atom(Module) -> 
	List = Module:module_info(attributes),
    proplists:get_value(vsn, List, "unknown").

%%******************************************************************************
%%*** ZOTONIC_SYSTEM ***********************************************************
%%******************************************************************************
%%------------------------------------------------------------------------------
%% @doc Main entry point
%% @end
%%------------------------------------------------------------------------------
zotonic_system(Opts) ->
    % Initializations
	zotonic_system_lib:set_verbosity(Opts),
	_Logfile     = zotonic_system_lib:set_stderr(Opts),
	_Output      = zotonic_system_lib:set_stdout(Opts),
	?DEBUG(Opts),
	% Check arguments dependances
	check_requirements(Opts),
	% Detect mode in arguments
	Mode = get_mode(Opts),
    case Mode of
     	list       -> zs_list(Opts);
     	categories -> zs_categories(Opts);
     	apps       -> zs_apps(Opts);
     	targets    -> zs_targets(Opts);
     	target     -> zs_target(Opts);
     	path       -> zs_path(Opts);
     	_          -> ?FATAL("Unknown argument")
    end,
	ok.

%%------------------------------------------------------------------------------
%% @doc -L, --list        List all (category/app/target)
%% @end
%%------------------------------------------------------------------------------
zs_list(_Opts) -> 
	L1 = zotonic_system_lib:get_depth_after_prefix("zotonic_system/priv/templates", 0),
	L = lists:usort(lists:flatmap(fun(X) -> [filename:dirname(X)] end, L1)),
	lists:foreach(fun(X) -> 
					?STDERR(X) 
					end, L),
	ok.

%%------------------------------------------------------------------------------
%% @doc -C, --categories  List all categories
%% @end
%%------------------------------------------------------------------------------
zs_categories(_Opts) -> 
	Cat = zotonic_system_lib:get_depth_after_prefix("zotonic_system/priv/templates", 1),
	lists:foreach(fun(X) -> 
					?STDERR(X) 
					end, Cat),
	ok.

%%------------------------------------------------------------------------------
%% @doc  -A, --apps        List all applications for a category (require -c)
%% @end
%%------------------------------------------------------------------------------
zs_apps(Opts)-> 
    {_, C} = lists:keyfind(category, 1, Opts),
    Prefix = filename:join(["zotonic_system/priv/templates", C]),
	Apps = zotonic_system_lib:get_depth_after_prefix(Prefix, 1),
	lists:foreach(fun(X) -> 
					?STDERR(X) 
					end, Apps),
	ok.

%%------------------------------------------------------------------------------
%% @doc -T, --targets     List all targets for a category and an application (require -a, -c).
%% @end
%%------------------------------------------------------------------------------
zs_targets(Opts)-> 
    {_, C} = lists:keyfind(category, 1, Opts),
    {_, A} = lists:keyfind(app, 1, Opts),
    Prefix = filename:join(["zotonic_system/priv/templates", C, A]),
	Targets = zotonic_system_lib:get_depth_after_prefix(Prefix, 1),
	lists:foreach(fun(X) -> 
					?STDERR(X) 
					end, Targets),
	ok.

%%------------------------------------------------------------------------------
%% @doc -t, --target      Target (require -c, -a)
%% @end
%%------------------------------------------------------------------------------
zs_target(Opts)-> 
    {_, C} = lists:keyfind(category, 1, Opts),
    {_, A} = lists:keyfind(app, 1, Opts),
    {_, T} = lists:keyfind(target, 1, Opts),
    Prefix = filename:join(["zotonic_system/priv/templates", C, A, T]),
	Targets = zotonic_system_lib:get_depth_after_prefix(Prefix, 0),
	erlang:display(Targets),
	ok.

%%------------------------------------------------------------------------------
%% @doc -p, --path        Path to a category/app/target (see -L output)
%% @end
%%------------------------------------------------------------------------------
zs_path(Opts)-> 
    {_, P} = lists:keyfind(path, 1, Opts),
    Prefix = filename:join(["zotonic_system/priv/templates", P]),
	Targets = zotonic_system_lib:get_depth_after_prefix(Prefix, 0),
	case (length(Targets) < 2 ) of
		true -> ?FATAL("Invalid package");
		false -> ok
	end,
	try
		% Read stdin
		Yaml = zotonic_system_lib:get_stdin(),
		% Get servername from config
		NodeName = 
		case re:run(Yaml, "# Zotonic config for (.*):", [{capture, all_but_first, list}]) of
			{match, Captured} -> Captured;
			_ -> ""
		end,
		Extra = [{node_name, lists:flatten(NodeName)}],
		% Start yamerl
		ok   = application:start(yamerl),
		% Parse Yaml
		C    = yamerl_constr:string(Yaml),
		% Get Zotonic conf as proplist
		ZConf = lists:flatten(C),
		?DEBUG(io_lib:format("~n~p",[ZConf])),
		% Merge all sources in a single one for rendering
		Conf  = zs_conf(ZConf, Extra),
		?INFO(io_lib:format("~n~p",[Conf])),
	    show(Prefix, Conf)
	catch
		_:R:S -> ?ERROR(R),
				 ?DEBUG(S)
	end,
	ok.

%%------------------------------------------------------------------------------
%% @doc Detect mode from command line arguments
%% @end
%%------------------------------------------------------------------------------
get_mode(Opts) ->
	try
    case lists:member(list, Opts)         of true -> throw(list);       _ -> ok end,
    case lists:member(categories, Opts)   of true -> throw(categories); _ -> ok end,
    case lists:member(apps, Opts)         of true -> throw(apps);       _ -> ok end,
    case lists:member(targets, Opts)      of true -> throw(targets);    _ -> ok end,
    case lists:keymember(target, 1, Opts) of true -> throw(target);     _ -> ok end,
    case lists:keymember(path, 1, Opts)   of true -> throw(path);       _ -> ok end
	catch
		_:R -> R
	end.


show(Path, Conf)
	-> 
	% Display abstract on stderr
	AbstFile = filename:join(Path, "abstract"),
	{ok,[{_, AbstractBin}]} = zotonic_system_lib:get_from_escript(AbstFile),
	Abstract = erlang:binary_to_list(AbstractBin),
	?STDERR(Abstract),
	c:flush(),
	% Get filename from Abstract
	FileName = 
		case re:run(Abstract, "Provides:(.*)", [{capture, all_but_first, list}]) of
			{match, Captured} -> Captured;
			_ -> ""
		end,
	% Display evaluated template on stdout
	File = filename:join(Path, "file"),
	{ok,[{_, CBin}]} = zotonic_system_lib:get_from_escript(File),
	%C = erlang:binary_to_list(CBin),
	{ok, Module} = template_compiler:compile_binary(CBin, FileName, [], []),
	?DEBUG({template_module, Module}),
	IOList = Module:render(Conf, [], []),
	?PRINT_FILE(FileName, io_lib:format("~ts",[IOList])),
	ok.

%%------------------------------------------------------------------------------
%% @doc Compose config map for templates
%% @end
%%------------------------------------------------------------------------------
zs_conf(ZConf, Extra) 
	-> 
	OSVars = {vars, lists:flatmap(fun(V) -> 
			  [L, R] = string:split(V, "="),
			  [{L, R}]
	         end, os:getenv())},
	OSInfo = {info, [lists:flatmap(fun(I) ->
			   case (catch erlang:system_info(I)) of
			   		{'EXIT', _} -> [];
			   		Z -> [{I,Z}]
			   	end
		      end, si_list())]},
	OSConf = {os, lists:flatten([OSInfo] ++ [OSVars])},
	lists:flatten([ZConf] ++ [OSConf] ++ [Extra]).

%%------------------------------------------------------------------------------
%% @doc List of erlang:system_info/1 options for templates
%%      @note : only system wide options should be used, and not options 
%%              specific to current zotonic_system escript
%% @end
%%------------------------------------------------------------------------------

si_list() ->
		[
         %allocated_areas
         %,allocator
         %,atom_count
         %,atom_limit
         build_type
         ,c_compiler_used
         %,check_io
         ,compat_rel
         ,cpu_topology
         %,creation
         ,debug_compiled
         %,delayed_node_table_gc
         %,dirty_cpu_schedulers
         %,dirty_cpu_schedulers_online
         %,dirty_io_schedulers
         %,dist
         %,dist_buf_busy_limit
         %,dist_ctrl
         ,driver_version
         %,dynamic_trace
         %,dynamic_trace_probes
         %,end_time
         %,elib_malloc
         %,eager_check_io
         %,ets_count
         %,ets_limit
         %,fullsweep_after
         %,garbage_collection
         %,heap_sizes
         %,heap_type
         %,info
         %,kernel_poll
         %,loaded
         ,logical_processors
         ,logical_processors_available
         ,logical_processors_online
         ,machine
         %,max_heap_size
         %,message_queue_data
         %,min_heap_size
         %,min_bin_vheap_size
         %,modified_timing_level
         %,multi_scheduling
         %,multi_scheduling_blockers
         ,nif_version
         %,normal_multi_scheduling_blockers
         ,otp_release
         %,os_monotonic_time_source
         %,os_system_time_source
         %,port_parallelism
         %,port_count
         %,port_limit
         %,process_count
         %,process_limit
         %,procs
         %,scheduler_bind_type
         %,scheduler_bindings
         %,scheduler_id
         %,schedulers
         %,schedulers_online
         ,smp_support
         %,start_time
         ,system_architecture
         %,system_logger
         %,system_version
         %,threads
         %,thread_pool_size
         %,time_correction
         %,time_offset
         %,time_warp_mode
         %,tolerant_timeofday
         %,trace_control_word
         %,update_cpu_info
         ,version
         ,wordsize
         ,{wordsize, internal}
         ,{wordsize, external}
         %,overview
         %,sequential_tracer
         ].




