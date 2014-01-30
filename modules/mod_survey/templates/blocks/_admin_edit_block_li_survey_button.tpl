{% extends "admin_edit_widget_i18n.tpl" %}

{% block widget_title %}{_ Block _}{% endblock %}
{% block widget_show_minimized %}false{% endblock %}
{% block widget_id %}edit-block-{{ name }}{% endblock %}
{% block widget_header %}{% endblock %}

{% block widget_content %}
{% with m.rsc[id] as r %}
    {% if is_editable %}
    <div class="control-group">
        <input type="text" id="block-{{name}}-prompt{{ lang_code_for_id }}" name="block-{{name}}-prompt{{ lang_code_with_dollar }}" 
               class="input-large" value="{{ blk.prompt[lang_code]  }}"
               placeholder="{_ Button text _} ({{ lang_code }})" />
    </div>

    <div class="control-group view-expanded">
       <textarea id="block-{{name}}-explanation{{ lang_code_for_id }}" name="block-{{name}}-explanation{{ lang_code_with_dollar }}" 
              class="input-block-level" rows="2"
              placeholder="{_ Explanation _} ({{ lang_code }})" >{{ blk.explanation[lang_code]  }}</textarea>
    </div>

    {% else %}
        <p>{{ blk.prompt[lang_code]  }}</p>
    {% endif %}
{% endwith %}
{% endblock %}

{% block widget_content_nolang %}
<div class="control-group view-expanded">
    <label class="control-label" for="block-{{name}}-style">{_ Button style _}</label>
    <div class="controls">
      <select id="block-{{name}}-style" name="block-{{name}}-style">
           <option value="">{_ Default _}</option>
           <option value="btn-primary" {% if blk.style == "btn-primary" %}selected="selected"{% endif %}>{_ Primary _}</option>
           <option value="btn-info" {% if blk.style == "btn-info" %}selected="selected"{% endif %}>{_ Informational _}</option>
           <option value="btn-success" {% if blk.style == "btn-success" %}selected="selected"{% endif %}>{_ Success _}</option>
           <option value="btn-warning" {% if blk.style == "btn-warning" %}selected="selected"{% endif %}>{_ Warning _}</option>
           <option value="btn-danger" {% if blk.style == "btn-danger" %}selected="selected"{% endif %}>{_ Danger _}</option>
           <option value="btn-inverse" {% if blk.style == "btn-inverse" %}selected="selected"{% endif %}>{_ Inverse _}</option>
      </select>
    </div>
</div>

<div class="control-group view-expanded">
    <label class="control-label" for="block-{{name}}-target">{_ question _}</label>
    <div class="controls">
      <input type="text" id="block-{{name}}-target" name="block-{{name}}-target" value="{{ blk.target }}" placeholder="{_ Jump target _}" />
      <p class="help-block">{_ Jump to a question on a next page. _}</p>
    </div>
</div>
{% endblock %}

