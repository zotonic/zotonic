.. include:: meta-postback.rst

Performs a custom server side validation of an input value. This allows you to
add your own validation logic to HTML form fields.

Start by adding a validator of the type ``postback`` to your form field in
your template:

.. code-block:: django

    <input type="text" id="username" name="username" value="" />
    {% validate id="username" type={postback event="validate_username"} %}

The ``event`` argument declares the name of the event that will be
:ref:`notified <guide-notification>`.
:ref:`Handle this event <handling-notifications>` in your site or module:

.. code-block:: erlang
    :caption: sites/yoursite/yoursite.erl

    -export([
        observe_validate_username/2
    ]).

    %% The event name passed in your template as event="validate_username",
    %% prefixed with observe_
    observe_validate_username({validate_username, {postback, Id, Value, _Args}}, Context) ->
        case is_valid(Value) of
            true ->
                {{ok, Value, Context};
            false ->
                %% The validation message will be shown in the form
                {{error, Id, "Sorry, that's not valid. Try again!"}, Context}
        end.

    %% Some function that returns true or false depending on the validity of the
    %% input value
    is_valid(Value) ->
        %% ...

.. seealso::

    * :ref:`guide-validators`
