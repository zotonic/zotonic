{#
    This file is for testing the indentation of the `zotonic-tpl-mode`.

    Run the tests by issuing `M-x zotonic-tpl-mode-test-buffer` on this buffer.

    Simply adding more text to this file will extend the test suite.
    Or, to test the indentation run the command on another buffer to test
    the indentation in that buffer.
#}

1. Single line (non block) template tag.
{% button text="Test button" %}

2. Multi line (non block) template tag.
{% button
    text="Test"
    action={growl message="Test button pressed"} %}

... and with the closing tag on it's own line:
{% button
    text="Test"
    action={growl message="Test button pressed"}
%}

3. Nesting actions on multiline (still non block) template tag.
{% button text=_"Create a new Project"
    class="btn btn-default btn-xs pull-right"
    action={dialog_new_rsc
        cat="project"
        nocatselect=1
        redirect=0
        action={postback
            delegate="trackz"
            postback={setup_project id}
            inject_args
        }
        action={insert_top
            target="projects"
            template="_project_list_entry.tpl"
        }
        action={hide target="no-projects-notice"}
    }
%}

4. simple template block tags
{% if true %}
    then this
{% else %}
    that.
{% endif %}

5. open block tag on multiple lines
{% if this=
        that %}
    then that
{% endif %}

... with closing tag on new line
{% if this=
        that
%}
    then foo
{% endif %}

6. simple tag soup test
<p>
    indent this para.
</p>

7. nested tag soup
<div class="foo">
    <p>
        paragraph in it...
    </p>
    trailer..
</div>
outside...

8. don't indent tag soup following a template tag
{% tabs id="tabs" %}
<div id="tabs">
    tab soup.
</div>

9. indent multiline soup tags
<div>
    <label for="{{ #title.id }}">Project name</label>
    <input class="form-control"
        type="text"
        id="{{ #title.id }}"
        name="title"
        value="{{ id.title }}" />
    {% validate id=#title.id name="title" type={presence} %}

    <textarea class="form-control"
        foo
    >
        Default text...
    </textarea>
</div>

10. don't indent after self closing soup tags
<input type="hidden" name="id" value="{{ id }}" />
<div class="ui-widget">
    form widget...
</div>

11. tag soup tend to be surrounded by ordinary text
Like this <strong>interesting</strong> word,
should not indent the following line.


12. endif tag on same line as ending self close soup tag
<input class="form-control"
    type="foo"
    {% if test %}
        bar="foo"
    {% endif %} />
Baz

{# Don't add tests below this point... #}
{# The test results are updated automatically by the test routine #}

{# Test results #}
All tests OK
{# End results #}

{# Local Variables: #}
{# tab-width: 4 #}
{# End: #}
