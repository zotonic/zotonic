
.. include:: meta-script.rst


This action executes JavaScript directly. It can be used to interface with non-Zotonic JavaScript libraries and functions.

Example::

   {% button text="hello" action={script script="alert('hello world')"} %}

Clicking on the button will show a JavaScript alert with the text `hello world` in it.

Using template variables::

    {% with "world" as recipient %}
        {% button
            text="hello"
            action={
                script
                script="alert('hello " ++ recipient ++ "')"
            }
        %}
    {% endwith %}
