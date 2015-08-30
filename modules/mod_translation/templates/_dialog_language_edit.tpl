{#
Params:
code
lang
fallback
#}
<p>{_ Enter the iso code (for example <em>en</em> or <em>nl</em>), and the title of the language. Use the native form of the language, for example <em>English</em>, <em>Türkçe</em> or <em>Français</em>. _}</p>

{% wire id=#form type="submit" postback={language_edit code=code} delegate="mod_translation" %}
<form id="{{ #form }}" method="POST" action="postback" class="form-horizontal">
    <div class="form-group row">
	    <label class="control-label col-md-3" for="{{ #code }}">{_ ISO Code _}</label>
        <div class="col-md-9">
            <select class="form-control" name="code" id="{{ #code }}">
                {% for iso,lang in m.translation.language_list_all %}
                    <option value="{{ iso }}" {% if iso == code %}selected{% endif %}>
                        {{iso}} ({{ lang.language }})
                    </option>
                {% endfor %}
            </select>
        </div>
    </div>

    <div class="form-group row">
	    <label class="control-label col-md-3" for="{{ #language }}">{_ Language _}</label>
        <div class="col-md-9">
	        <input class="form-control" type="text" id="{{ #language }}" name="language" value="{{ lang.language }}" />
	        {% validate id=#language name="language" type={presence} %}
        </div>
    </div>
    
    <div class="form-group row">
        <div class="clearfix">{# clean wrap for hint #}
            <label class="control-label col-md-3" for="{{ #fallback }}">{_ Fallback language _}</label>
            <div class="col-md-9">
                <select class="form-control" name="fallback" id="{{ #fallback }}">
                    <option value="">{_ none _}</option>
                    {% for iso,lang in m.translation.language_list_all %}
                        <option value="{{ iso }}" {% if iso == fallback %}selected{% endif %}>
                            {{iso}} ({{ lang.language }})
                        </option>
                    {% endfor %}
                </select>
            </div>
        </div>
        <div class="col-md-3"></div>
        <div class="col-md-9">
            <p class="help-block">{_ Use this with region-specific languages, for instance 'fr-be'. The fallback language is used to find missing translations in the 'parent' language instead of substituting with English. _}</p>
        </div>
    </div>
    
    <div class="form-group row">
	    <label class="control-label col-md-3" for="{{ #enabled }}">{_ Show in menu _}</label>
        <div class="col-md-9">
            <div class="checkbox"><label>
	                <input type="checkbox" id="{{ #enabled }}" name="is_enabled" value="1"
	                    {% if new or lang.is_enabled %}checked="checked"{% endif %} />
                </label></div>
        </div>
    </div>

    <div class="modal-footer">
        {% button class="btn btn-default" action={dialog_close} text=_"Cancel" tag="a" %}
        <button class="btn btn-primary" type="submit">{_ Save _}</button>
    </div>
</form>

