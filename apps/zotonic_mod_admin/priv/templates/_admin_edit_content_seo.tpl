{% extends "admin_edit_widget_i18n.tpl" %}

{# Widget for editing SEO preferences #}

{% block widget_title %}
{_ SEO Content _}
<div class="widget-header-tools"></div>
{% endblock %}

{% block widget_show_minimized %}true{% endblock %}
{% block widget_id %}edit-seo{% endblock %}

{% block widget_content %}
<fieldset>
    <div class="form-group row">
        <label class="control-label col-md-3">{_ Page slug _} {{ lang_code_with_brackets }}</label>
        <div class="col-md-9">
            <input type="text"
                id="title_slug{{ lang_code_for_id }}"
                name="title_slug{{ lang_code_with_dollar }}"
                value="{{ is_i18n|if : id.translation[lang_code].title_slug : id.title_slug }}"
                {% if not id.custom_slug or not id.is_editable %}disabled{% endif %}
                {% include "_language_attrs.tpl" language=lang_code class="input-xlarge form-control input-slug" %}
            />
        </div>
    </div>

    <div class="form-group row">
        <label class="control-label col-md-3">{_ Page title _} {{ lang_code_with_brackets }}</label>
        <div class="col-md-9">
            <input type="text"
                id="seo_title{{ lang_code_for_id }}"
                name="seo_title{{ lang_code_with_dollar }}"
                value="{{ is_i18n|if : id.translation[lang_code].seo_title : id.seo_title }}"
                {% if not id.is_editable %}disabled{% endif %}
                {% include "_language_attrs.tpl" language=lang_code class="form-control" %}
            />
        </div>
    </div>

    <div class="form-group row">
        <label class="control-label col-md-3">{_ Page keywords _} {{ lang_code_with_brackets }}</label>
        <div class="col-md-9">
            <input type="text"
                id="seo_keywords{{ lang_code_for_id }}"
                name="seo_keywords{{ lang_code_with_dollar }}"
                value="{{ is_i18n|if : id.translation[lang_code].seo_keywords : id.seo_keywords }}"
                {% if not id.is_editable %}disabled{% endif %}
                {% include "_language_attrs.tpl" language=lang_code class="form-control" %}
            />
        </div>
    </div>

    <div class="form-group row">
        <label class="control-label col-md-3">{_ Page description _} {{ lang_code_with_brackets }}</label>
        <div class="col-md-9">
            <textarea rows="5" cols="10"
                id="seo_desc{{ lang_code_for_id }}{{ lang_code_for_id }}"
                name="seo_desc{{ lang_code_with_dollar }}"
                {% if not id.is_editable %}disabled{% endif %}
                {% include "_language_attrs.tpl" language=lang_code class="seo-desc form-control" %}
            >{{ is_i18n|if : id.translation[lang_code].seo_desc : id.seo_desc }}</textarea>
        </div>
    </div>
</fieldset>
{% endblock %}

{% block widget_content_nolang %}
<fieldset class="form-horizontal">
    {% with m.rsc[id] as r %}
        <div class="form-group row">
            <div class="col-md-9 col-md-offset-3">
                <div class="checkbox">
                    <label>
                        <input
                            id="custom-slug"
                            type="checkbox"
                            name="custom_slug"
                            {% if id.custom_slug %}checked="checked"{% endif %}
                            {% if not id.is_editable %}disabled{% endif %}
                            value="1"
                            onclick="$('#custom-slug:checked').val() ? $('.input-slug').removeAttr('disabled') : $('.input-slug').attr('disabled', 'disabled');"
                        />
                        {_ Customize page slug _}
                    </label>
                </div>
            </div>
        </div>

        <div class="form-group row">
            <div class="col-md-9 col-md-offset-3">
                <div class="checkbox">
                    <label>
                        <input id="seo_noindex"
                            type="checkbox"
                            name="seo_noindex"
                            value="1"
                            {% if not id.is_editable %}disabled{% endif %}
                            {% if id.seo_noindex %}checked="checked"{% endif %}
                        />
                        {_ Ask google to not index this page _}
                    </label>
                </div>
            </div>
        </div>

        {% all catinclude "_admin_edit_content_seo_extra.tpl" r.id %}
    {% endwith %}
</fieldset>
{% endblock %}
