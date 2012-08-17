{% extends "admin_edit_widget_i18n.tpl" %}

{% block widget_title %}{_ Block _}{% endblock %}
{% block widget_show_minimized %}false{% endblock %}
{% block widget_id %}edit-block-{{ name }}{% endblock %}

{% block widget_content %}
{% with m.rsc[id] as r %}
<fieldset class="admin-form">
    <div class="control-group">
    {% if is_editable %}
        <label>{_ Button text _}</label>
        <input type="text" id="block-{{name}}-prompt{{ lang_code_with_dollar }}" name="block-{{name}}-prompt{{ lang_code_with_dollar }}" 
               class="span8" value="{{ blk.prompt[lang_code] }}"
               placeholder="{_ Please enter your name. _} ({{ lang_code }})" />

       <label>{_ Explanation text (optional) _}</label>
       <textarea id="block-{{name}}-explanation{{ lang_code_with_dollar }}" name="block-{{name}}-explanation{{ lang_code_with_dollar }}" 
              class="span8" rows="2"
              placeholder="{_ Explanation _} ({{ lang_code }})" >{{ blk.explanation[lang_code] }}</textarea>

    {% else %}
        <p>{{ blk.prompt[lang_code] }}</p>
    {% endif %}
    </div>
</fieldset>
{% endwith %}
{% endblock %}

{% block widget_content_nolang %}
<fieldset class="form-vertical">
    <label for="block-{{name}}-style">{_ Button style _}</label>
    <select id="block-{{name}}-style" name="block-{{name}}-style">
         <option value="">{_ Default _}</option>
         <option value="btn-primary" {% if blk.style == "btn-primary" %}selected="selected"{% endif %}>{_ Primary _}</option>
         <option value="btn-info" {% if blk.style == "btn-info" %}selected="selected"{% endif %}>{_ Informational _}</option>
         <option value="btn-success" {% if blk.style == "btn-success" %}selected="selected"{% endif %}>{_ Success _}</option>
         <option value="btn-warning" {% if blk.style == "btn-warning" %}selected="selected"{% endif %}>{_ Warning _}</option>
         <option value="btn-danger" {% if blk.style == "btn-danger" %}selected="selected"{% endif %}>{_ Danger _}</option>
         <option value="btn-inverse" {% if blk.style == "btn-inverse" %}selected="selected"{% endif %}>{_ Inverse _}</option>
    </select>

    <div class="control-group">
        <label for="block-{{name}}-target">{_ Name of target question _}</label>
        <input type="text" id="block-{{name}}-target" name="block-{{name}}-target" value="{{ blk.target }}" placeholder="{_ Jump target _}" />
    </div>
</fieldset>
{% endblock %}

