{% extends "admin_edit_widget_std.tpl" %}

{% block widget_header %}{% endblock %}
{% block widget_show_minimized %}false{% endblock %}
{% block widget_id %}edit-block-{{ name }}{% endblock %}

{% block widget_content %}
{% with m.rsc[id] as r %}
    {% if is_editable %}
    <div class="row-fluid">
        <div class="span6">
            <label>{_ Page jump condition _}</label>
            <input type="text" id="block-{{name}}-condition1" name="block-{{name}}-condition1" 
                   class="input-block-level" value="{{ blk.condition1 }}"
                   placeholder="{_ name >= 2 _}" />
        </div>
        <div class="span4">
            <label>{_ To question _}</label>
            <input type="text" id="block-{{name}}-target1" name="block-{{name}}-target1" 
                   class="input-block-level" value="{{ blk.target1 }}"
                   placeholder="{_ name _}" />
        </div> 
    </div>
    <div class="row-fluid">
        <div class="span6">
            <input type="text" id="block-{{name}}-condition2" name="block-{{name}}-condition2" 
                   class="input-block-level" value="{{ blk.condition2 }}"
                   placeholder="{_ name >= 2 _}" />
        </div>
        <div class="span4">
            <input type="text" id="block-{{name}}-target2" name="block-{{name}}-target2" 
                   class="input-block-level" value="{{ blk.target2 }}"
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
