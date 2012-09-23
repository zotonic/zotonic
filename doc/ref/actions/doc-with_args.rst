
Apply actions with arguments added.

This action takes a list of other actions. One or more arguments are added to the actions before the actions are executed.  This action is mostly used in included templates or callbacks.  An example can be seen with the :ref:`action-typeselect` action.

Another example, assume we have a template "_list_action.tpl"::

   {% for id in list %}
     <li><a id="{{ #list.id }}" href="#">{{ m.rsc[id].title }}</a></li>
     {% wire id=#list.id action={with_args action=my_action arg={id id}} %}
   {% endfor %}

Then we can pass an action to this template::

   {% include "_list_action.tpl" list=[1,2,3,4,5] my_action={redirect dispatch="admin_edit_rsc"} %}

The result will be a list of titles for the pages with id 1..5.  Every title will be a link to its admin page, as the argument `id` will be added to the `my_action`.
