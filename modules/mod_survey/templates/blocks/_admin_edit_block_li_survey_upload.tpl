{% extends "admin_edit_widget_i18n.tpl" %}

{% block widget_title %}{_ Block _}{% endblock %}
{% block widget_show_minimized %}false{% endblock %}
{% block widget_id %}edit-block-{{ name }}{% endblock %}
{% block widget_header %}{% endblock %}

{% block widget_content %}
{% with m.rsc[id] as r %}
    <p class="alert view-expanded">
        {_ This block show a file upload field. This can only be used as the last question of a survey. The default survey routines can’t handle file upload, you will need to add your own survey handler to your site or module. _}
    </p>

    {% if is_editable %}
    <div class="control-group">
        <input type="text" id="block-{{name}}-prompt{{ lang_code_for_id }}" name="block-{{name}}-prompt{{ lang_code_with_dollar }}" 
               class="input-block-level" value="{{ blk.prompt[lang_code]  }}"
               placeholder="{_ Please upload your image. _} ({{ lang_code }})" />
    </div>
    {% else %}
        <p>{{ blk.prompt[lang_code]  }}</p>
    {% endif %}
{% endwith %}
{% endblock %}

{% block widget_content_nolang %}
    <div class="view-expanded view-expanded">
        <div class="control-group">
            <label class="checkbox">
                <input type="checkbox" id="block-{{name}}-is_image" name="block-{{name}}-is_image" value="1" {% if blk.is_image or is_new %}checked="checked"{% endif %} />
                {_ Only accept images. _}
            </label>
            <label class="checkbox">
                <input type="checkbox" id="block-{{name}}-is_required" name="block-{{name}}-is_required" value="1" {% if blk.is_required or is_new %}checked="checked"{% endif %} />
                {_ Required, this question must be answered. _}
            </label>
        </div>
    </div>
{% endblock %}

