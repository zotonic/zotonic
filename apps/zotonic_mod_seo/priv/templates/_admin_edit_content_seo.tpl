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
    <div class="form-group label-floating">
        <input type="text"
            id="title_slug{{ lang_code_for_id }}"
            name="title_slug{{ lang_code_with_dollar }}"
            value="{{ is_i18n|if : id.translation[lang_code].title_slug : id.title_slug }}"
            {% if not id.custom_slug or not id.is_editable %}disabled{% endif %}
            {% include "_language_attrs.tpl" language=lang_code class="input-xlarge form-control input-slug" %}
            placeholder="{_ Page slug _} {{ lang_code_with_brackets }}"
        >
        <label class="control-label">{_ Page slug _} {{ lang_code_with_brackets }}</label>
    </div>

    <div class="form-group label-floating">
        <input type="text"
            id="seo_title{{ lang_code_for_id }}"
            name="seo_title{{ lang_code_with_dollar }}"
            value="{{ is_i18n|if : id.translation[lang_code].seo_title : id.seo_title }}"
            {% if not id.is_editable %}disabled{% endif %}
            {% include "_language_attrs.tpl" language=lang_code class="form-control" %}
            placeholder="{_ Page title _} {{ lang_code_with_brackets }}"
        >
        <label class="control-label">{_ Page title _} {{ lang_code_with_brackets }}</label>
    </div>

    <div class="form-group label-floating">
        <input type="text"
            id="seo_keywords{{ lang_code_for_id }}"
            name="seo_keywords{{ lang_code_with_dollar }}"
            value="{{ is_i18n|if : id.translation[lang_code].seo_keywords : id.seo_keywords }}"
            {% if not id.is_editable %}disabled{% endif %}
            {% include "_language_attrs.tpl" language=lang_code class="form-control" %}
            placeholder="{_ Page keywords _} {{ lang_code_with_brackets }}"
        >
        <label class="control-label">{_ Page keywords _} {{ lang_code_with_brackets }}</label>
    </div>

    <div class="form-group label-floating">
        <textarea rows="5"
            id="seo_desc{{ lang_code_for_id }}"
            name="seo_desc{{ lang_code_with_dollar }}"
            {% if not id.is_editable %}disabled{% endif %}
            {% include "_language_attrs.tpl" language=lang_code class="seo-desc form-control field-collapse" %}
            placeholder="{_ Page description _} {{ lang_code_with_brackets }}"
        >{{ is_i18n|if : id.translation[lang_code].seo_desc : id.seo_desc }}</textarea>
        <label class="control-label col-md-3">{_ Page description _} {{ lang_code_with_brackets }}</label>
    </div>

    <div class="form-group label-floating">
        <textarea rows="10"
            id="seo_ld_json{{ lang_code_for_id }}"
            name="seo_ld_json{{ lang_code_with_dollar }}"
            {% if not id.is_editable %}disabled{% endif %}
            {% include "_language_attrs.tpl" language=lang_code class="form-control field-collapse" %}
            placeholder="{_ Custom JSON-LD _} {{ lang_code_with_brackets }}"
        >{{ is_i18n|if : id.translation[lang_code].seo_ld_json : id.seo_ld_json }}</textarea>
        <label class="control-label col-md-3">{_ Custom JSON-LD _} {{ lang_code_with_brackets }}</label>
        <p class="help-block">
            {_ Add your custom JSON-LD for structured metadata. This replaces the matching parts of the automatically generated JSON-LD. _}
            <a href="https://developers.google.com/search/docs/appearance/structured-data/intro-structured-data" target="_blank" rel="noreferrer noopener">{_ More information [Google] _}</a>
        </p>
        {% validate id="seo_ld_json"++lang_code_for_id
                    name="seo_ld_json"++lang_code_with_dollar
                    type={json}
        %}
    </div>

    <details>
        <summary>{_ Show current JSON-LD (updates after save) _} {{ lang_code_with_brackets }}</summary>
        {% live template="_seo_admin_preview_json_ld.tpl" topic=id id=id lang_code=lang_code %}
    </details>
</fieldset>
{% endblock %}

{% block widget_content_nolang %}
    {% with id.category_id.is_seo_noindex_cat as is_seo_noindex_cat %}
    <fieldset>
        <div class="form-group">
            <label class="checkbox">
                <input
                    id="custom-slug"
                    type="checkbox"
                    name="custom_slug"
                    {% if id.custom_slug %}checked="checked"{% endif %}
                    {% if not id.is_editable %}disabled{% endif %}
                    value="1"
                >
                {_ Customize page slug _}
            </label>
            {% javascript %}
                $('#custom-slug').click(function(e) {
                    if ($(this).is(":checked")) {
                        $('.input-slug').removeAttr('disabled');
                    } else {
                        $('.input-slug').attr('disabled', 'disabled');
                    }
                });
            {% endjavascript %}
        </div>

        <div class="form-group">
            <label class="checkbox">
                <input id="seo_noindex"
                    type="checkbox"
                    name="seo_noindex"
                    value="1"
                    {% if not id.is_editable or is_seo_noindex_cat %}disabled{% endif %}
                    {% if id.seo_noindex or is_seo_noindex_cat %}checked="checked"{% endif %}
                >
                {_ Ask Google to not index this page _}
            </label>

            {% if is_seo_noindex_cat %}
                <p class="help-block">
                    {% trans "SEO indexing of this page is disabled because it is in the “{cat}” category."
                            cat=id.category_id.title
                    %}
                    &nbsp; <a href="{% url admin_edit_rsc id=id.category_id %}">{_ Edit _}</a>
                </p>
            {% endif %}
        </div>
    </fieldset>
    {% endwith %}

    {% if id.is_a.category %}
        <fieldset>
            <legend>{_ All pages of this category _}</legend>
            <p class="help-block">
                {_ Pages of this category can be excluded from search engines. This is useful if some categories should not be found on Google (et al). Common examples are categories and predicates. _}
            </p>
            <div class="form-group">
                <label class="checkbox">
                    <input id="is_seo_noindex_cat"
                        type="checkbox"
                        name="is_seo_noindex_cat"
                        value="1"
                        {% if not id.is_editable %}disabled{% endif %}
                        {% if id.is_seo_noindex_cat %}checked="checked"{% endif %}
                    >
                    {% trans "Ask Google to exclude all “{cat}” pages" cat=id.title %}
                </label>
            </div>
        </fieldset>
    {% endif %}

    {% all catinclude "_admin_edit_content_seo_extra.tpl" id %}
</fieldset>
{% endblock %}
