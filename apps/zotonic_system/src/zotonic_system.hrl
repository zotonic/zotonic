%%% zotonic_system.hrl

%% Macro to test command args
-define(TEST_HELP(O, Args, A), case getopt:parse(O, Args) of
       {ok, {Options, NonOptArgs}} ->
           % Help ?
           case proplists:is_defined(help, Options) of
               true  -> throw({help, A});
               _     -> ok
			  end,
           % Version ?
           case proplists:is_defined(version, Options) of
               true  -> throw({version, A});
               _     -> ok
			  end,
           NewOptions =
                        case proplists:is_defined(file, Options) of
                           true -> % Replace "-f -" by filename from stdin, if needed
                                   F0    = proplists:get_value(file, Options, ""),
                                   File = case F0 of
                                           "stdin"  -> string:strip(zotonic_system_lib:get_stdin(),right,$\n) ;
                                           "-"      -> string:strip(zotonic_system_lib:get_stdin(),right,$\n) ;
                                           F0       -> F0
                                          end,
                                   NoFileList = proplists:delete(file, Options),
                                   NoFileList ++ [{file, File}] ;
                           false -> Options
                        end,
                        throw({A, NewOptions, NonOptArgs})
           ;
       {error, {_, _}} -> skip
   end).

%% Macros for logging
% Levels : 
%   0   : prints raw on stdout
%   1-9 : prints on stderr as a log
%   10- : prints raw on stderr 
%
%   emergency = 1
%   alert     = 2
%   critical  = 3
%   error     = 4
%   warning   = 5
%   notice    = 6
%   info      = 7
%   debug     = 8
%   trace     = 9
-define(PRINT(S, L), case (get(verbosity) >= L) of
        true -> display(S, L) ;
        false -> ok end ).

-define(EMERGENCY(S), display(S, 1)).
-define(ALERT(S),     display(S, 2)).
-define(CRITICAL(S),  display(S, 3)).
-define(ERROR(S),     display(S, 4)).
-define(WARNING(S),   display(S, 5)).
-define(NOTICE(S),    display(S, 6)).
-define(INFO(S),      display(S, 7)).
-define(DEBUG(S),     display(S, 8)).
-define(TRACE(S),     display(S, 9)).

-define(STDOUT(S),    display(S, 0)).
-define(STDERR(S),    display(S, 10)).

%% Macro for fatal error
-define(FATAL(S), display(S, 4), halt(1)).

%% display log
display(S, L) when (L < 0) -> 
  case ( - get(verbosity) =< L ) of
        true  -> E = get(stderr), io:format(E,format(S, L),[fold(S)]) ;
        false -> skip
  end;
display(S, L) when (L==0)   -> O = get(stdout), io:format(O,format(S, L),[fold(S)]) ;
display(S, L) when (L==10)  -> E = get(stderr), io:format(E,format(S, L),[fold(S)]) ;
display(S, L) -> case (get(verbosity) >= L) of 
                    true  -> E = get(stderr), io:format(E,format(S, L),[fold(S)]);
                    false -> ok
                 end.

%% safe fold
fold(S) when is_list(S) -> lists:flatten(S);
fold(S)                 -> S.

%% format depending it is printable or not
format(S, L) when is_list(S) -> 
  case io_lib:printable_unicode_list(lists:flatten(S)) of
		 true  -> timeinfo() ++ prefix(L) ++ "~ts~n" ;
		 false -> "~p.~n"
  end;
format(S, L) -> 
  case io_lib:printable_unicode_list(S) of
		 true  -> timeinfo() ++ prefix(L) ++ "~ts~n" ;
		 false -> "~p.~n"
  end.

%% prefix for log levels
prefix(1) -> "<!> ";
prefix(2) -> "{!} ";
prefix(3) -> "(!) ";
prefix(4) -> "[!] ";
prefix(5) -> "/!\\ ";
prefix(6) -> "--> ";
prefix(7) -> "(i) ";
prefix(8) -> " #  ";
prefix(9) -> " @  ";
prefix(_) -> "".

%% display timeinfo starting log level 6
timeinfo() -> 
  case (get(verbosity) >= 8 ) of
   false -> [] ;
   true  -> zotonic_system_lib:format_date()++ " - "
  end.

%% Print content on stdout with markers on stderr, title free
-define(PRINT_FILE(F, C),
  display("---8<--- " ++ filename:basename(F) ++ " --------\n", 10), % stderr
  c:flush(),
  display(C,0), % stdout
  c:flush(),
  display("---8<----" ++ string:copies("-", string:len(filename:basename(F)))++ "---------\n", 10) %stderr
).

%% Print filecontent on stdout and filename in markers on stderr
-define(PRINT_FILE(F),
  display("---8<--- " ++ filename:basename(F) ++ " --------\n", 10),
  c:flush(),
  {ok, Binary} = file:read_file(F),
  display(erlang:binary_to_list(Binary),0),
  c:flush(),
  display("---8<----" ++ string:copies("-", string:len(filename:basename(F)))++ "---------\n", 10)
).

