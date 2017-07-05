
.. include:: meta-mod_logging.rst

Logs messages to the database and adds log views to the admin.

Logging messages to the database
--------------------------------

To persist a log message in the database, enable mod_logging in your Zotonic
site. Then, in your code, send the ``#zlog{}`` notification::

    -include_lib("zotonic_core/include/zotonic.hrl").

    some_function() ->
        %% do some things

        z_notifier:notify(
            #zlog{
                user_id = z_acl:user(Context),
                props=#log_email{
                    severity = ?LOG_ERROR,
                    message_nr = MsgId,
                    mailer_status = bounce,
                    mailer_host = z_convert:ip_to_list(Peer),
                    envelop_to = BounceEmail,
                    envelop_from = "<>",
                    to_id = z_acl:user(Context),
                    props = []
            }},
            Context
        );

E-mail log
----------

The e-mail log is a separate view, which lists which email messages
have been sent to which recipients. Any mail that gets sent gets
logged here.

.. seealso::

    For regular application logging, use :ref:`Lager <dev-logging>` instead.

.. todo:: Add more documentation
