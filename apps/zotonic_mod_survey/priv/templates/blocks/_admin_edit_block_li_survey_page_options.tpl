{% extends "admin_edit_widget_std.tpl" %}

{% block widget_header %}{% endblock %}
{% block widget_show_minimized %}false{% endblock %}
{% block widget_id %}edit-block-{{ name }}{% endblock %}

{% block widget_content %}
    <p class="help-block">
        {_ This block contains options for a page. The presence of this block signals the end of the questions for a page. _}
    </p>

    {% if id.is_editable %}
    <div class="row">
        <div class="col-md-12">
            <label class="checkbox">
                <input type="checkbox" name="is_stop_page" {% if blk.is_stop_page %}checked{% endif %}>
                {_ Remove "Next" button. No answers will be submitted unless you add a button jump to a next page. _}
            </label>
            <label class="checkbox">
                <input type="checkbox" name="options.is_hide_back" {% if blk.is_hide_back %}checked{% endif %}>
                {_ Remove "Back" button. If there is instant question feedback then it will be shown after submit. _}
            </label>
        </div>
    </div>
    {% endif %}
{% endblock %}
