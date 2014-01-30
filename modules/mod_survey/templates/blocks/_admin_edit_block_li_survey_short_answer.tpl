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
               class="input-block-level" value="{{ blk.prompt[lang_code]  }}"
               placeholder="{_ Please enter your name. _} ({{ lang_code }})" />
    </div>

    <div class="control-group view-expanded">
        <textarea id="block-{{name}}-explanation{{ lang_code_for_id }}" name="block-{{name}}-explanation{{ lang_code_with_dollar }}" 
               class="input-block-level" rows="2"
               placeholder="{_ Explanation _} ({{ lang_code }})" >{{ blk.explanation[lang_code]  }}</textarea>
       </div>

    <div class="control-group view-expanded">
        <input type="text" id="block-{{name}}-placeholder{{ lang_code_for_id }}" name="block-{{name}}-placeholder{{ lang_code_with_dollar }}" 
               class="input-block-level" value="{{ blk.placeholder[lang_code]  }}"
               placeholder="{_ Input value placeholder text _} ({{ lang_code }})" />
    </div>
       
    {% else %}
        <p>{{ blk.prompt[lang_code]  }}</p>
    {% endif %}
{% endwith %}
{% endblock %}

{% block widget_content_nolang %}
    <div class="control-group view-expanded">
        <label class="control-label" for="block-{{name}}-validation">{_ Validation _}</label>
        <div class="controls">
            <select id="block-{{name}}-validation" name="block-{{name}}-validation">
                 <option value=""></option>
                 <option value="email" {% if blk.validation == "email" %}selected="selected"{% endif %}>{_ must be an e-mail address _}</option>
                 <option value="numericality" {% if blk.validation == "numericality" %}selected="selected"{% endif %}>{_ must be a number _}</option>
                 <option value="phone" {% if blk.validation == "phone" %}selected="selected"{% endif %}>{_ must be a phone number _}</option>
                 <option value="date" {% if blk.validation == "date" %}selected="selected"{% endif %}>{_ must be a date _}</option>
            </select>
        </div>
    </div>

    <div class="control-group view-expanded">
        <label class="checkbox">
            <input type="checkbox" id="block-{{name}}-is_required" name="block-{{name}}-is_required" value="1" {% if blk.is_required or is_new %}checked="checked"{% endif %} />
            {_ Required, this question must be answered. _}
        </label>
    </div>
{% endblock %}

