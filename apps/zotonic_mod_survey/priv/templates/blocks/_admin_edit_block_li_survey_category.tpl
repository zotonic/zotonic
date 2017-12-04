{% extends "admin_edit_widget_i18n.tpl" %}

{% block widget_title %}
{_ Block _}
<div class="widget-header-tools"></div>
{% endblock %}

{% block widget_show_minimized %}false{% endblock %}
{% block widget_id %}edit-block-{{ name }}{% endblock %}
{% block widget_header %}{% endblock %}

{% block widget_content %}
    {% if id.is_editable %}
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
{% endblock %}

{% block widget_content_nolang %}
<div class="row view-expanded">
    <div class="col-lg-6 col-md-6 question-options">
      <div class="form-group">
          <div class="radio">
              <label>
                  <input type="radio" id="block-{{name}}-input_type" name="block-{{name}}-input_type" value="" {% if not blk.input_type %}checked="checked"{% endif %} />
                  {_ Single answer possible _}
              </label>
          </div>
          <div class="radio">
              <label>
                  <input type="radio" id="block-{{name}}-input_type" name="block-{{name}}-input_type" value="multi" {% if blk.input_type == 'multi' %}checked="checked"{% endif %} />
                  {_ Multiple answers possible _}
              </label>
          </div>
          <div class="radio">
              <label>
                  <input type="radio" id="block-{{name}}-input_type" name="block-{{name}}-input_type" value="submit" {% if blk.input_type == 'submit' %}checked="checked"{% endif %} />
                  {_ Submit on clicking an option _}
              </label>
          </div>

          <div class="checkbox">
              <label>
                  <input type="checkbox" id="block-{{name}}-is_required" name="block-{{name}}-is_required" value="1" {% if blk.is_required or is_new %}checked="checked"{% endif %} />
                  {_ Required, this question must be answered. _}
              </label>
          </div>

          <div class="checkbox">
              <label>
                  <input type="checkbox" id="block-{{name}}-is_hide_result" name="block-{{name}}-is_hide_result" value="1" {% if blk.is_hide_result %}checked="checked"{% endif %} />
                  {_ Hide from results _}
              </label>
          </div>
      </div>
    </div>

    <div class="col-lg-6 col-md-6">
        <div class="form-group">
          <label class="control-label">{_ Category to choose from _}</label>
          <select class="form-control" id="block-{{name}}-category" name="block-{{name}}-category">
              <option></option>
              {% for c in m.category.tree_flat %}
                <option value="{{ c.id.name }}" {% if blk.category == c.id.name %}selected="selected"{% endif %}>
                    {{ c.indent }}{{ c.id.title|default:c.id.name }}
                </option>
              {% endfor %}
          </select>
        </div>
    </div>
</div>
{% endblock %}

