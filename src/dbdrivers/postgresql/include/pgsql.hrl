% Timeouts for query, start pool and pool calls.
-define(PGSQL_TIMEOUT, 600000).
-define(PGSQL_START_TIMEOUT, 60000).
-define(PGSQL_CALL_TIMEOUT,  600000).


-record(column,    {name, type, size, modifier, format}).
-record(statement, {name, columns, types}).

-record(error,  {severity, code, message, extra}).
