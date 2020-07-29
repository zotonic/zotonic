%%%-----------------------------------------------------------------------------
%%% File:      zotonic_system_lib.erl
%%% @author    Eric Pailleau <zotonic@crownedgrouse.com>
%%% @copyright 2020 Eric Pailleau
%%% @doc  Library for zotonic_system
%%% @end
%%%-----------------------------------------------------------------------------
-module(zotonic_system_lib).
-author("Eric 'crownedgrouse' Pailleau <zotonic@crownedgrouse.com>").

-export([get_from_escript/1, get_depth_after_prefix/2]).
-export([format_time/1, format_time/0, format_date/0]).
-export([get_stdin/0, get_stdin/1, set_verbosity/1, set_stderr/1, set_stdout/1]).
-export([tostr/1]).

-include("zotonic_system.hrl").

tostr(T) when is_integer(T)->  integer_to_list(T);
tostr(T) when is_atom(T)   ->  atom_to_list(T);
tostr(T) when is_binary(T) ->  binary_to_list(T);
tostr(T) when is_list(T)   ->  case io_lib:printable_list(T) of
												true  -> T ;
												false -> io_lib:format("~p", [T])
										 end;
tostr(T) ->  io_lib:format("~p", [T]).


localtime_ms() ->
    {_, _, Micro} = Now = os:timestamp(),
    {Date, {Hours, Minutes, Seconds}} = calendar:now_to_local_time(Now),
    {Date, {Hours, Minutes, Seconds, Micro div 1000 rem 1000}}.

format_time() ->
    format_time(localtime_ms()).

format_time({{Y, M, D}, {H, Mi, S, Ms}}) ->
    {io_lib:format("~b-~2..0b-~2..0b", [Y, M, D]),
     io_lib:format("~2..0b:~2..0b:~2..0b.~3..0b", [H, Mi, S, Ms])};
format_time({{Y, M, D}, {H, Mi, S}}) ->
    {io_lib:format("~b-~2..0b-~2..0b", [Y, M, D]),
     io_lib:format("~2..0b:~2..0b:~2..0b", [H, Mi, S])}.

format_date() -> {L, R} = format_time(),
					  io_lib:format("~s ~s", [L, R]).

%%------------------------------------------------------------------------------
%% @doc Get files with a prefix from escript itself
%% @end
%%------------------------------------------------------------------------------
get_from_escript(Prefix) ->
  {ok, [{shebang,_}, {comment,_}, {emu_args,_},
          {archive, ArchiveBin}]} = escript:extract(escript:script_name(), []),
  {ok, Archive} = zip:extract(ArchiveBin, [memory]),
  Fun = fun({Path, _}) -> lists:prefix(Prefix, Path) end,
  Sources_ = lists:filter(Fun, Archive),
  Sources  = lists:flatmap(fun({Path, _}) -> [Path] end, Sources_),
  X = zip:extract(ArchiveBin, [memory, {file_list, Sources}]),
  X.


%%------------------------------------------------------------------------------
%% @doc Get stdin
%% @end
%%------------------------------------------------------------------------------
get_stdin()  -> get_stdin([]).

get_stdin(F) ->  
    {ok, TRef} = timer:exit_after(2000, self(),"No data in stdin"),
    case io:get_chars('', 8192) of
       eof -> timer:cancel(TRef), F ;
       T   -> timer:cancel(TRef), get_stdin(F ++ T)
    end.
%%------------------------------------------------------------------------------
%% @doc Set verbosity
%%      Default to Warning
%% @end
%%------------------------------------------------------------------------------
set_verbosity(Opts) ->
   V = case catch erlang:list_to_integer(proplists:get_value(verbose, Opts, "5")) of
         X when is_integer(X) -> X ;
         _                    -> 5
      end,
   put(verbosity, V).

%%------------------------------------------------------------------------------
%% @doc Set logfile target (stderr by default)
%% @end
%%------------------------------------------------------------------------------
set_stderr(Opts)    ->
   L = case proplists:get_value(logfile, Opts, "stderr") of
         "stderr"          -> standard_error ;
         "stdout"          -> standard_io ;
         X when is_list(X) -> case file:open(X, [write, append]) of
                                    {ok, IoDevice}  -> IoDevice ;
                                    {error, Reason} -> throw("Error : Opening " ++ X ++ " : " ++ zotonic_system_lib:tostr(Reason))
                              end
      end,
   put(stderr, L),
   L.

%%------------------------------------------------------------------------------
%% @doc Set output target (stdout by default)
%% @end
%%------------------------------------------------------------------------------
set_stdout(Opts)    ->
   L = case proplists:get_value(output, Opts, "stdout") of
         "stderr"          -> standard_error ;
         "stdout"          -> standard_io ;
         X when is_list(X) ->file:delete(X),  % Remove if any
                 case file:open(X, [write, exclusive, raw, delayed_write]) of
                                    {ok, IoDevice}  -> IoDevice ;
                                    {error, Reason} -> throw("Error : Opening " ++ X ++ " : " ++ zotonic_system_lib:tostr(Reason))
                              end
      end,
   put(stdout, L),
   L.

%%------------------------------------------------------------------------------
%% @doc Extract files at a given depth under a path prefix, inside escript 
%% @end
%%------------------------------------------------------------------------------
get_depth_after_prefix(Prefix, Depth)
  when is_list(Prefix), is_integer(Depth), (Depth > 0)
  -> 
  Rel = get_depth_after_prefix(Prefix, 0),
  % Get path part at given depth
  Fun2 = fun(Path, Acc) ->
    Acc ++ [lists:nth(Depth, filename:split(Path))]
  end,
  lists:usort(lists:foldl(Fun2, [], Rel));

get_depth_after_prefix(Prefix, 0)
  when is_list(Prefix)
  ->
  L = case zotonic_system_lib:get_from_escript("zotonic_system/priv/templates") of
      {error, R}  -> ?FATAL(R) ;
      {ok, Y}   -> Y 
      end,
  % Get relative path after prefix
  Fun1 = fun({Path, _}, Acc) -> 
    case lists:prefix(Prefix, Path) of
      false -> [] ;
      true  -> PrS = filename:split(Prefix),
           PaS = filename:split(Path),
           Acc ++ [filename:join(PaS -- PrS)]
    end
  end,
  lists:usort(lists:foldl(Fun1, [], lists:sort(L))).