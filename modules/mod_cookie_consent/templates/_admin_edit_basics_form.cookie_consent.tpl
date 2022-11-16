{% with m.rsc[id] as r %}
{% with not id or m.rsc[id].is_editable as is_editable %}
<fieldset class="form-horizontal">
    <div class="form-group row">
        <label class="control-label col-md-3" for="{{ #title }}{{ lang_code_for_id }}">{_ Title _} {{ lang_code_with_brackets }}</label>
        <div class="col-md-9">
            <input type="text" id="{{ #title }}{{ lang_code_for_id }}" name="title{{ lang_code_with_dollar }}"
                   value="{{ is_i18n|if : r.translation[lang_code].title : r.title }}"
                   {% if not is_editable %}disabled="disabled"{% endif %}
                   {% include "_language_attrs.tpl" language=lang_code class="do_autofocus field-title form-control" %}
            />
        </div>
    </div>

    <div class="form-group row">
        <label class="control-label col-md-3" for="{{ #cookie_explanation }}{{ lang_code_for_id }}">{_ Cookie explanation _}{{ lang_code_with_brackets }}</label>
        <div class="col-md-9">
            <textarea rows="4" cols="10" id="{{ #cookie_explanation }}{{ lang_code_for_id }}" name="cookie_explanation{{ lang_code_with_dollar }}" {% if not is_editable %}disabled="disabled"{% endif %} {% include "_language_attrs.tpl" language=lang_code class="intro form-control" %}>{{ is_i18n|if : r.translation[lang_code].cookie_explanation : r.cookie_explanation | brlinebreaks }}</textarea>
	    </div>
    </div>

    <hr/>

    <h5 style="color:grey">{_ Detailed explanation of cookie types. Leave a field empty if you don't want to this type of cookie _}</h5>

    <div class="form-group row">
        <label class="control-label col-md-3" for="{{ #cookies_functional }}{{ lang_code_for_id }}">{_ Functional _}{{ lang_code_with_brackets }}</label>
        <div class="col-md-9">
            <textarea rows="4" cols="10" id="{{ #cookies_functional }}{{ lang_code_for_id }}" name="cookies_functional{{ lang_code_with_dollar }}" {% if not is_editable %}disabled="disabled"{% endif %} {% include "_language_attrs.tpl" language=lang_code class="intro form-control" %}>{{ is_i18n|if : r.translation[lang_code].cookies_functional : r.cookies_functional | brlinebreaks }}</textarea>
	    </div>
    </div>

    <div class="form-group row">
        <label class="control-label col-md-3" for="{{ #cookies_preference }}{{ lang_code_for_id }}">{_ Preference _}{{ lang_code_with_brackets }}</label>
        <div class="col-md-9">
            <textarea rows="4" cols="10" id="{{ #cookies_preference }}{{ lang_code_for_id }}" name="cookies_preference{{ lang_code_with_dollar }}" {% if not is_editable %}disabled="disabled"{% endif %} {% include "_language_attrs.tpl" language=lang_code class="intro form-control" %}>{{ is_i18n|if : r.translation[lang_code].cookies_preference : r.cookies_preference | brlinebreaks }}</textarea>
	    </div>
    </div>

        <div class="form-group row">
        <label class="control-label col-md-3" for="{{ #cookies_statistics }}{{ lang_code_for_id }}">{_ Statistics _}{{ lang_code_with_brackets }}</label>
        <div class="col-md-9">
            <textarea rows="4" cols="10" id="{{ #cookies_statistics }}{{ lang_code_for_id }}" name="cookies_statistics{{ lang_code_with_dollar }}" {% if not is_editable %}disabled="disabled"{% endif %} {% include "_language_attrs.tpl" language=lang_code class="intro form-control" %}>{{ is_i18n|if : r.translation[lang_code].cookies_statistics : r.cookies_statistics | brlinebreaks }}</textarea>
	    </div>
    </div>

        <div class="form-group row">
        <label class="control-label col-md-3" for="{{ #cookies_marketing }}{{ lang_code_for_id }}">{_ Marketing _}{{ lang_code_with_brackets }}</label>
        <div class="col-md-9">
            <textarea rows="4" cols="10" id="{{ #cookies_marketing }}{{ lang_code_for_id }}" name="cookies_marketing{{ lang_code_with_dollar }}" {% if not is_editable %}disabled="disabled"{% endif %} {% include "_language_attrs.tpl" language=lang_code class="intro form-control" %}>{{ is_i18n|if : r.translation[lang_code].cookies_marketing : r.cookies_marketing | brlinebreaks }}</textarea>
	    </div>
    </div>
</fieldset>

{% endwith %}
{% endwith %}