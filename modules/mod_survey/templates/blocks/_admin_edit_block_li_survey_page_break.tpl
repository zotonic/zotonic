{% extends "admin_edit_widget_std.tpl" %}

{% block widget_header %}{% endblock %}
{% block widget_show_minimized %}false{% endblock %}
{% block widget_id %}edit-block-{{ name }}{% endblock %}

{% block widget_content %}
{% with m.rsc[id] as r %}
<fieldset class="form-vertical">
    <div class="control-group">
    {% if is_editable %}
    <div class="row">
        <div class="span5">
            <label>{_ Page jump condition _}</label>
            <input type="text" id="block-{{name}}-condition1" name="block-{{name}}-condition1" 
                   class="span5" value="{{ blk.condition1 }}"
                   placeholder="{_ name >= 2 _}" />
        </div>
        <div class="span2">
            <label>{_ To question _}</label>
            <input type="text" id="block-{{name}}-target1" name="block-{{name}}-target1" 
                   class="span2" value="{{ blk.target1 }}"
                   placeholder="{_ name _}" />
        </div> 

        <div class="span5">
            <input type="text" id="block-{{name}}-condition2" name="block-{{name}}-condition2" 
                   class="span5" value="{{ blk.condition2 }}"
                   placeholder="{_ name >= 2 _}" />
        </div>
        <div class="span2">
            <input type="text" id="block-{{name}}-target2" name="block-{{name}}-target2" 
                   class="span2" value="{{ blk.target2 }}"
                   placeholder="{_ name _}" />
        </div>
    </div>
    <p class="help-block">
        {_ When no condition is true then the question following this break will be the next page. _}
        {_ Multiple page break blocks are merged into one. _}
    </p>

    <div class="control-group">
        <label class="checkbox">
            <input type="checkbox" id="block-{{name}}-is_nocount" name="block-{{name}}-is_nocount" value="1" {% if blk.is_not_counted %}checked="checked"{% endif %} />
            {_ This page break doesn’t count towards the number of questions. _}
        </label>
    </div>
    {% endif %}
    </div>
</fieldset>
{% endwith %}
{% endblock %}
