
.. include:: meta-typeselect.rst


Show possible selections whilst typing.

Performs a search for the typed text whilst typing in an input field.  Shows possible matching pages in a selectable list.

Example::

   <form method="get" action="/search">
     <input type="text" id="person" name="person" value="" />
     <ul id="suggestions"></ul>
     <input type="hidden" id="person_id" value="" />
     {% wire id="person" type="keyup"
             action={typeselect cat="person" 
                               target="suggestions"
                               action_with_id={with_args action={set_value target="person_id"} arg={value select_id}}
                               action={submit}} 
     %}
   </form>

This is a rather complicated example. It connects the typeahead action to the input element. The list of suggestions will be shown in the `<ul/>` with id `suggestions`.  Only pages in the category `person` will be found.

The listed suggestions will have two actions attached. One action will set the value of the hidden input element `person_id` to the id of the selected suggestion (which is a :term:`page`). The other action will submit the form.

The `action_with_id` arguments are always performed before the `action` arguments.

The `typeselect` action accepts the following arguments:

==============  ================================================================  =======
Argument        Description                                                       Example
==============  ================================================================  =======
target          The id of element that will show the list of suggestions.         target="mylist"
cat             The category for the searched pages.
                This argument can be repeated.                                    cat="text"
template        Template used to show the list of possible pages. 
                This defaults to the template "_action_typeselect_result.tpl".
                The template gets the following arguments: 
                result (list of ids), action_with_id and action.                  template="_show_suggestions.tpl"
action_with_id  Actions executed when a suggestion is selected. 
                The id of the selected page will be added as the `id` parameter.
                This argument can be repeated.                                    action_with_id={postback postback="page_select"}
action          Actions executed when a suggestion is selected.  
                This list is executed after the `action_with_id` actions.
                This argument can be repeated.                                    action={slide_up target="form-id"}
==============  ================================================================  =======
