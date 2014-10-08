{% extends "admin_edit_widget_std.tpl" %}

{% block widget_header %}{% endblock %}
{% block widget_show_minimized %}false{% endblock %}
{% block widget_id %}edit-block-{{ name }}{% endblock %}

{% block widget_content %}
{% with m.rsc[id] as r %}
    {% if is_editable %}
    <div class="row">
        <div class="col-lg-6 col-md-6">
            <label>{_ Page jump condition _}</label>
            <input class="form-control" type="text" id="block-{{name}}-condition1" name="block-{{name}}-condition1" value="{{ blk.condition1 }}"
                   placeholder="{_ name >= 2 _}" />
        </div>
        <div class="col-lg-4 col-md-4">
            <label>{_ To question _}</label>
            <input class="form-control" type="text" id="block-{{name}}-target1" name="block-{{name}}-target1" value="{{ blk.target1 }}"
                   placeholder="{_ name _}" />
        </div> 
    </div>
    <div class="row">
        <div class="col-lg-6 col-md-6">
            <input class="form-control" type="text" id="block-{{name}}-condition2" name="block-{{name}}-condition2" value="{{ blk.condition2 }}"
                   placeholder="{_ name >= 2 _}" />
        </div>
        <div class="col-lg-4 col-md-4">
            <input class="form-control" type="text" id="block-{{name}}-target2" name="block-{{name}}-target2" value="{{ blk.target2 }}"
                   placeholder="{_ name _}" />
        </div>
    </div>
    <p class="help-block">
        {_ When no condition is true then the question following this break will be the next page. _}
        {_ Multiple page break blocks are merged into one. _}
    </p>
    {% endif %}
{% endwith %}
{% endblock %}
