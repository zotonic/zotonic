
.. highlight:: django
.. include:: meta-hasedge.rst

A :ref:`validator <guide-validators>` to check if a resource has a certain number
of edges with a predicate.

An example of its usage is an edit form where the article needs between 1 and 5 keywords and an author.

It can be attached to any (hidden) input field or the form itself.

An example to check the number of keywords attached to a resource::

    <div class="form-group"> <!-- form-group has class "has-error" if validation fails -->
        <input type="hidden" id="check-author" value="author">
        {% validate id="check-author"
                    type={hasedge id=id minimum=1}
                    only_on_submit
        %}
        <p class="if-field-invalid" style="display: none">{_ You must have at least one author. _}</p>
        <p class="if-field-valid" style="display: none">{_ Great, you added at least one author. _}</p>
    </div>

On form submit the check will be done. And either of the two messages will be shown.
The predicate to be checked is the value of the hidden input element.

The hidden input element is not submitted as it doesn’t have a name.


Arguments
---------

===============  =================================================  =======
Argument         Description                                        Example
===============  =================================================  =======
id               Subject resource id for the edge check.            ``id``
predicate        Predicate to check, defaults to the value of the   ``"author"``
                 input element the validator is connected to.
minimum          Minimum amount of edged allowed, defaults to 1.    ``2``
maximum          Maximum amount of edged allowed, per default no    ``10``
                 maximum.
===============  =================================================  =======
