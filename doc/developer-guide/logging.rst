.. _dev-logging:

Logging
=======

.. seealso:: :ref:`cookbook-logstash` cookbook
.. seealso:: To log messages to the database, you can use :ref:`mod_logging`.

Zotonic uses `Logger`_ for logging. Logger metadata is automatically set by
Zotonic in the controller functions. 

To log a message from your code, simply call Logger directly:

.. code-block:: erlang

    -include_lib("kernel/include/logger.hrl").

    ?LOG_ERROR("Something went very wrong with ~p", [SomeParam]).

which will write the following in the :file:`error.log` file:

.. code-block:: none

    2022-01-28 17:17:45 ERROR <0.9.0> [some_module:some_function/0:42] ▸ text="Something went very wrong with whatever"


Configuration
-------------

In :ref:`erlang-config` file, change the ``kernel`` section to configure log
handlers and formatters. See the `Logger documentation`_ for more information.

For instance, to configure Logger for logging to Logstash, uncomment the ``logstash``
handler in the logger configuration of your ``erlang.config`` file and check the
``logstasher`` configuration:

.. code-block:: erlang
    :caption: erlang.config


    {kernel, [
        % Minimum log level for all loggers below.
        {logger_level, info},

        {logger, [

            %% To use logstash:
            %% - Enable the logstasher_h handler
            %% - Configure logstasher (see below the kernel config)
            %%
            % {handler, logstash, logstasher_h,
            %     #{
            %         level => info
            %     }
            % },

            % Log of all information to the terminal/console.
            % The 'logger_level' above defines what is shown on the console.
            {handler, default, z_logger_h,
                #{
                    level => debug,
                    formatter => {logjam,
                        #{
                            prettify => true,
                            colored => true,
                            time_designator => $\s,
                            time_offset => "",
                            time_unit => second,
                            strip_tz => true,
                            level_capitalize => true
                        }
                    }
                }
            },

            % Error log on disk.
            % LOG_DIR is replaced with the default Zotonic ZOTONIC_LOG_DIR directory.
            {handler, error_log, z_logger_h,
                #{
                    level => error,
                    config => #{
                        type => file,
                        file => "LOG_DIR/error.log",
                        max_no_files => 10,
                        max_no_bytes => 52428800 % 10 x 5mb
                    },
                    formatter => {logjam,
                        #{
                            prettify => true,
                            colored => false,
                            time_designator => $\s,
                            time_offset => "",
                            time_unit => second,
                            strip_tz => true,
                            level_capitalize => true
                        }
                    }
                }
            },

            % Console log on disk.
            % LOG_DIR is replaced with the default Zotonic ZOTONIC_LOG_DIR directory.
            {handler, console_log, z_logger_h,
                #{
                    level => debug,
                    config => #{
                        type => file,
                        file => "LOG_DIR/console.log",
                        max_no_files => 10,
                        max_no_bytes => 52428800 % 10 x 5mb
                    },
                    formatter => {logjam,
                        #{
                            prettify => true,
                            colored => false,
                            time_designator => $\s,
                            time_offset => "",
                            time_unit => second,
                            strip_tz => true,
                            level_capitalize => true
                        }
                    }
                }
            }
        ]}
    ]},

    %% Logstash configuration.
    %% If a logger handler with 'logstasher_h' is defined then zotonic_core will start the
    %% logstasher application.
    {logstasher, [
        {transport, console}, % tcp | udp | console
        {host, "localhost"},  % inet:hostname()
        {port, 5000}          % inet:port_number()
    ]},


.. _Logger: https://www.erlang.org/doc/man/logger.html
.. _Logger documentation: https://www.erlang.org/doc/apps/kernel/logger_chapter.html
