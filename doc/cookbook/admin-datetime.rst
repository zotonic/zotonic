Storing date/time fields
========================

Some interesting tidbits about saving/updating a date/time field of a
rsc.

Why
---

The purpose of this guide is to explain how Zotonic stores date-time
data so that you can work with it in your own modules.

Assumptions
-----------

Readers are expected to be interested in authoring Zotonic Modules.
To be successful you should be fairly experienced with Erlang
programming.

How
---

When updating a :term:`resource` by passing a `proplist` to
``m_rsc:update/3``, Zotonic has a way of looking for specific name
patterns in the proplist that tells it what items are date/time fields
and which ones are not. This allows Zotonic to split those off the
proplists and use the name to derive information about the date, such
as expected format, if the default time should be 00:00 or 23:59, and
the actual field name that should be updated.

Here is an example format: ``dt:ymd:0:date_end``

Literally this means ...

- the characters ``dt:``, signifying the date-time marker
- followed by some year, month, date pattern or whatever combination
- followed by ":"
- followed by 1 or 0 to designate default 00:00 or 23:59
- followed by ":"
- followed by the field name to by updated.

Have a look at ``_admin_edit_date.tpl`` in the default site templates tosee this and other formatting options.

For example passing ``[{"dt:ymd:0:date_end", "2011-04-05"}]`` to ``m_rsc:update/3`` would update the date_end field.

Here is an example event callback::

  event({submit, {edit_details, [{id, Id}]}, _TriggerId, _TargetId}, Context) -> 
      End_date = z_context:get_q("dt:ymd:0:date_end", Context), 
      Props1 = [{"dt:ymd:0:date_end", End_date}], 
      case m_rsc:update(Id, Props1, Context) of 
                 {ok, _} -> 
                    z_render:wire({reload, []}, Context); 
                 {error, _} -> 
                     z_render:growl_error("Could not update record.", Context) 
      end. 

Of course the input field of your form could be named whatever you
wish, but when you assign it to the proplist you would use the naming
convention above.

Internals
---------

``m_rsc:update`` eventually hands off to ``m_rsc_update:update``. In
``m_rsc_upate``, zotonic passes the RscProps list to ``recombine_dates/3``
where it does a pattern match and list accumulator process.

Here is a description of the recombine_dates process::

  recombine_dates([], Dates, Acc) ->
      % When our Props list is empty, return the list of
      % Dates we created and Acc containing the list of
      % non-date tuples. 
      {Dates, Acc}; 
  recombine_dates([{"dt:"++K,V}|T], Dates, Acc) ->
      % if the head of the list is a tuple with 
      % {"dt:" plus some additional string with a value,
      % assign K to the rest of the sting and V to the
      % passed value. 
      [Part, End, Name] = string:tokens(K, ":"),
      % With those values in hand, split K on ":" and
      % assign the tokens to Part, End, Name 
      Dates1 = recombine_date(Part, End, Name, V, Dates),
      % Send our date 'packet' off for some more processing
      % and assign the return to 
      % Date1 so we can start over again. 
      recombine_dates(T, Dates1, Acc);
      % Call our function again withthe tail of our list 
  recombine_dates([H|T], Dates, Acc) ->
      % If the head of the list doesn't match "{"dt:"....}
      % we just pop it onto the head of the accumulator 
      recombine_dates(T, Dates, [H|Acc]).

There is more to this process, but this is all you need to get started.

