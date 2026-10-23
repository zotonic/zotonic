{% extends "admin_edit_widget_i18n.tpl" %}

{# Rights are shared across languages; provenance text can be translated. #}

{% block widget_id %}copyright{% endblock %}

{% block widget_title %}
    {_ Rights, attribution and provenance _}
{% endblock %}

{% block widget_show_minimized %}true{% endblock %}

{% block widget_content %}
<div class="form-group">
    <label class="control-label" for="rsc-provenance_html{{ lang_code_for_id }}">
        {_ Provenance _} {{ lang_code_with_brackets }}
    </label>
    <p class="help-block">{_ Describe the source, collection or archive reference. Keep names and identifiers unchanged when translating. _}</p>
    <textarea rows="4" cols="10" id="rsc-provenance_html{{ lang_code_for_id }}"
        name="provenance_html{{ lang_code_with_dollar }}"
        class="z_editor-init form-control"
        {% if id and not id.is_editable %}disabled{% endif %}
        {% include "_language_attrs.tpl" language=lang_code %}>{{ is_i18n|if:id.translation[lang_code].provenance_html:id.provenance_html | escape }}</textarea>
</div>
{% endblock %}

{% block widget_content_nolang_before %}

<p>
    {_ The license and the attribution of this page. _}<br>
    {_ <b>NB</b> connected pages and images have their own rights. _}
</p>

<div class="form-group">
    <label class="control-label">{_ License _}</label>
    <select name="rights" class="form-control">
        <option></option>
        <option value="CR" {% if id.rights == 'CR' %}selected{% endif %}>© {_ All Rights Reserved _}</option>
        <option value="PD" {% if id.rights == 'PD' %}selected{% endif %}>{_ Public Domain _}</option>
        <optgroup label="Creative Commons">
            {% for lic in m.copyright.list.creative_commons %}
                <option value="{{ lic.name }}" {% if lic.name == id.rights %}selected{% endif %}>
                    {{ lic.title }}
                </option>
            {% endfor %}
        </optgroup>
        <optgroup label="Rights Statements">
            {% for lic in m.copyright.list.rights_statements %}
                <option value="{{ lic.name }}" {% if lic.name == id.rights %}selected{% endif %}>
                    {{ lic.title }}
                </option>
            {% endfor %}
        </optgroup>
    </select>
</div>

<div class="form-group label-floating">
    <input type="text" name="rights_attribution" class="form-control" value="{{ id.rights_attribution }}" placeholder="{_ Attribution _}">
    <label class="control-label">{_ Attribution _}</label>
</div>

<div class="form-group label-floating">
{% with id.publication_start|default:id.created|date:"Y" as y %}
    <input type="text" name="rights_year" class="form-control" value="{{ id.rights_year }}" placeholder="{{ y }} ({_ Year _})">
    <label class="control-label">{{ y }} ({_ Year _})</label>
{% endwith %}
</div>

<p class="help-block">{_ More about licenses: _}
    <a href="https://creativecommons.org/share-your-work/cclicenses/" target="_blank" rel="noopener noreferrer">Creative Commons</a> |
    <a href="https://rightsstatements.org/page/1.0/" target="_blank" rel="noopener noreferrer">Rights Statements</a>
</p>
{% endblock %}

{% block widget_content_nolang %}
<div class="form-group label-floating">
    <input type="url" id="{{ #provenance_url }}" name="provenance_url" class="form-control"
        value="{{ id.provenance_url }}" placeholder="{_ Source URL _}"
        {% if id and not id.is_editable %}disabled{% endif %}>
    <label class="control-label" for="{{ #provenance_url }}">{_ Source URL _}</label>
    <p class="help-block">{_ Link to the original source record. This link is shared by all languages. _}</p>
</div>
{% endblock %}
