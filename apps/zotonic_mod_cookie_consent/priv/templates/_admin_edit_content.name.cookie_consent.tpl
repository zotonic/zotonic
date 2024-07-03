{% extends "admin_edit_widget_i18n.tpl" %}

{# Widget for editing SEO preferences #}

{% block widget_title %}
{_ Cookie consent settings _}
<div class="widget-header-tools"></div>
{% endblock %}

{% block widget_show_minimized %}true{% endblock %}
{% block widget_id %}edit-cookie-consent{% endblock %}

{% block widget_content %}
{% with not id or m.rsc[id].is_editable as is_editable %}
<fieldset class="form-horizontal">
    <p class="help-block">{_ Detailed explanation of cookie types. Leave a field empty if you don't want to show this type of cookie _}</p>

    <div class="form-group row">
        <label class="control-label col-md-3" for="{{ #cookies_functional }}{{ lang_code_for_id }}">{_ Functional _} {{ lang_code_with_brackets }}</label>
        <div class="col-md-9">
            <textarea rows="4" cols="10" id="{{ #cookies_functional }}{{ lang_code_for_id }}" name="cookies_functional{{ lang_code_with_dollar }}" {% if not is_editable %}disabled="disabled"{% endif %} {% include "_language_attrs.tpl" language=lang_code class="intro form-control" %}>{{ is_i18n|if : id.translation[lang_code].cookies_functional : id.cookies_functional | brlinebreaks }}</textarea>
        </div>
    </div>

    <div class="form-group row">
        <label class="control-label col-md-3" for="{{ #cookies_statistics }}{{ lang_code_for_id }}">{_ Statistics _} {{ lang_code_with_brackets }}</label>
        <div class="col-md-9">
            <textarea rows="4" cols="10" id="{{ #cookies_statistics }}{{ lang_code_for_id }}" name="cookies_statistics{{ lang_code_with_dollar }}" {% if not is_editable %}disabled="disabled"{% endif %} {% include "_language_attrs.tpl" language=lang_code class="intro form-control" %}>{{ is_i18n|if : id.translation[lang_code].cookies_statistics : id.cookies_statistics | brlinebreaks }}</textarea>
        </div>
    </div>

    <div class="form-group row">
        <label class="control-label col-md-3" for="{{ #cookies_marketing }}{{ lang_code_for_id }}">{_ Marketing _} {{ lang_code_with_brackets }}</label>
        <div class="col-md-9">
            <textarea rows="4" cols="10" id="{{ #cookies_marketing }}{{ lang_code_for_id }}" name="cookies_marketing{{ lang_code_with_dollar }}" {% if not is_editable %}disabled="disabled"{% endif %} {% include "_language_attrs.tpl" language=lang_code class="intro form-control" %}>{{ is_i18n|if : id.translation[lang_code].cookies_marketing : id.cookies_marketing | brlinebreaks }}</textarea>
        </div>
    </div>
</fieldset>

<p class="help-block">{_ To add more information about the use of cookies, either use the content field below or add a 'relation' in the page connections. _}</p>

{% endwith %}
{% endblock %}

{% block widget_content_nolang %}
{% endblock %}
