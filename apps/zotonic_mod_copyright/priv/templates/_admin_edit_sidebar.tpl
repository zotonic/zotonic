{% extends "admin_edit_widget_std.tpl" %}

{# editing license and attribution #}

{% block widget_id %}copyright{% endblock %}

{% block widget_title %}
    {_ License and attribution _}
{% endblock %}

{% block widget_show_minimized %}true{% endblock %}

{% block widget_content %}

<p>
    {_ The license and the attribution of this page. _}<br>
    {_ <b>NB</b> connected pages and images have their own rights. _}
</p>

<div class="form-group">
    <label class="control-label">{_ License _}</label>
    <select name="rights" class="form-control">
        <option></option>
        <option value="CR">© {_ All Rights Reserved _}</option>
        <option value="PD">{_ Public Domain _}</option>
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
