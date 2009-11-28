% Milliseconds for default query timeout.
-define(PGSQL_TIMEOUT, 60000).

-record(column,    {name, type, size, modifier, format}).
-record(statement, {name, columns, types}).

-record(error,  {severity, code, message, extra}).
