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
        <input type="text" id="block-{{name}}-prompt{{ lang_code_for_id }}"
               name="blocks[].prompt{{ lang_code_with_dollar }}"
               class="form-control" value="{{ blk.prompt|translation:lang_code }}"
               placeholder="{_ Button text _} ({{ lang_code }})">
    </div>

    <div class="form-group">
       <textarea class="form-control" id="block-{{name}}-explanation{{ lang_code_for_id }}"
                 name="blocks[].explanation{{ lang_code_with_dollar }}" rows="2"
                 placeholder="{_ Explanation _} ({{ lang_code }})" >{{ blk.explanation|translation:lang_code }}</textarea>
    </div>

    {% else %}
        <p>{{ blk.prompt|translation:lang_code }}</p>
    {% endif %}
{% endblock %}

{% block widget_content_nolang %}
<div class="row">
    <div class="col-md-5 form-group view-expanded">
        <label class="control-label" for="block-{{name}}-style">{_ Button style _}</label>
        <div>
          <select class="form-control" id="block-{{name}}-style" name="blocks[].style">
               <option value="btn-default">{_ Default _}</option>
               <option value="btn-primary" {% if blk.style == "btn-primary" %}selected="selected"{% endif %}>{_ Primary _}</option>
               <option value="btn-info" {% if blk.style == "btn-info" %}selected="selected"{% endif %}>{_ Informational _}</option>
               <option value="btn-success" {% if blk.style == "btn-success" %}selected="selected"{% endif %}>{_ Success _}</option>
               <option value="btn-warning" {% if blk.style == "btn-warning" %}selected="selected"{% endif %}>{_ Warning _}</option>
               <option value="btn-danger" {% if blk.style == "btn-danger" %}selected="selected"{% endif %}>{_ Danger _}</option>
               <option value="btn-inverse" {% if blk.style == "btn-inverse" %}selected="selected"{% endif %}>{_ Inverse _}</option>
          </select>
        </div>
    </div>
    <div class="col-md-4 form-group">
        <button type="button" id="{{ #livebutton }}" style="margin-top: 3rem;" class="btn {{ blk.style|default:"btn-default" }}">
            {_ Button preview _}
        </button>
    </div>
</div>

{% javascript %}
    document.getElementById('block-{{name}}-style')
            .addEventListener('change', function(){
                document.getElementById('{{ #livebutton }}').className = 'btn '+ this.value;
            });
{% endjavascript %}

<div class="form-group view-expanded question-options label-floating">
    <input class="form-control" type="text" id="block-{{name}}-target" name="blocks[].target" value="{{ blk.target }}" placeholder="{_ Question label _}" />
    <label class="control-label" for="block-{{name}}-target">{_ Jump to this question label (must be on an other page below) _}</label>
    <p class="help-block">{_ Jump to a question on a next page. _} {% trans "Use “{submit}” to create a submit button, or “{stop}” to create a stop button." submit="submit" stop="stop" %}</p>
</div>

<div class="form-group question-options">
    <div class="checkbox">
        <label>
            <input type="checkbox" id="block-{{name}}-is_hide_result" name="blocks[].is_hide_result" value="1" {% if blk.is_hide_result %}checked="checked"{% endif %} />
            {_ Hide from results _}
        </label>
    </div>
</div>

{% endblock %}

