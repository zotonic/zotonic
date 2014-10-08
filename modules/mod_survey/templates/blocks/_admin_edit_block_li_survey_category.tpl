{% extends "admin_edit_widget_i18n.tpl" %}

{% block widget_title %}{_ Block _}{% endblock %}
{% block widget_show_minimized %}false{% endblock %}
{% block widget_id %}edit-block-{{ name }}{% endblock %}
{% block widget_header %}{% endblock %}

{% block widget_content %}
{% with m.rsc[id] as r %}
    {% if is_editable %}
    <div class="form-group">
        <input class="form-control" type="text" id="block-{{name}}-prompt{{ lang_code_for_id }}" name="block-{{name}}-prompt{{ lang_code_with_dollar }}" value="{{ blk.prompt[lang_code]  }}"
               placeholder="{_ Prompt _} ({{ lang_code }})" />
    </div>
    <div class="form-group view-expanded">
       <textarea class="form-control" id="block-{{name}}-explanation{{ lang_code_for_id }}" name="block-{{name}}-explanation{{ lang_code_with_dollar }}" rows="2"
              placeholder="{_ Explanation _} ({{ lang_code }})" >{{ blk.explanation[lang_code]  }}</textarea>
    </div>
    {% else %}
        <p>{{ blk.prompt[lang_code]  }}</p>
    {% endif %}
{% endwith %}
{% endblock %}

{% block widget_content_nolang %}
<div class="row view-expanded">
    <div class="col-lg-6 col-md-6">
      <div class="form-group">
          <div class="radio"><label>
              <input type="radio" id="block-{{name}}-input_type" name="block-{{name}}-input_type" value="" {% if not blk.input_type %}checked="checked"{% endif %} />
              {_ Single answer possible _}
          </label></div>
          <div class="radio"><label>
              <input type="radio" id="block-{{name}}-input_type" name="block-{{name}}-input_type" value="multi" {% if blk.input_type == 'multi' %}checked="checked"{% endif %} />
              {_ Multiple answers possible _}
          </label></div>
          <div class="radio"><label>
              <input type="radio" id="block-{{name}}-input_type" name="block-{{name}}-input_type" value="submit" {% if blk.input_type == 'submit' %}checked="checked"{% endif %} />
              {_ Submit on clicking an option _}
          </label></div>

          <div class="checkbox"><label>
              <input type="checkbox" id="block-{{name}}-is_required" name="block-{{name}}-is_required" value="1" {% if blk.is_required or is_new %}checked="checked"{% endif %} />
              {_ Required, this question must be answered. _}
          </label></div>
      </div>
    </div>

    <div class="col-lg-6 col-md-6">
        <div class="form-group">
          <label>{_ Category to choose from _}</label>
          <select class="form-control" id="block-{{name}}-category" name="block-{{name}}-category">
              <option></option>
              {% for cat_id, level, indent, name in m.category.all_flat %}
              <option value="{{ name }}" {% if blk.category == name %}selected="selected"{% endif %}>
                  {{ indent }}{{ cat_id.title|default:name }}
              </option>
              {% endfor %}
          </select>
        </div>
    </div>
</div>
{% endblock %}

