{% if is_form_row %}
    <div class="form-group row">
        <label class="control-label col-md-3" for="{{ #medium_language }}">{_ Language of media content _}</label>
        <div class="col-md-9">
            <select id="{{ #medium_language }}" name="medium_language" class="form-control" style="width: auto">
                <option></option>
                {% with m.config.i18n.language_list.list|default:[[z_language,[]]] as list %}
                {% for code,lang in list %}
                    {% if lang.is_enabled %}
                        <option value="{{ code }}" {% if id.medium_language == code %}selected{% endif %}>
                            {{ lang.language|default:code }}
                        </option>
                    {% endif %}
                {% endfor %}
                {% endwith %}
            </select>

            <span class="help-block">
                {_ Some images, PDFs, audio and video are presented in a single language. Here you can select this language. _}
            </span>
        </div>
    </div>
{% else %}
    <div class="form-group">
        <label class="control-label" for="{{ #medium_language }}">
            {_ Language of media content _}
        </label>
        <select id="{{ #medium_language }}" name="medium_language" class="form-control" style="width: auto">
            <option></option>
            {% with m.config.i18n.language_list.list|default:[[z_language,[]]] as list %}
            {% for code,lang in list %}
                {% if lang.is_enabled %}
                    <option value="{{ code }}" {% if id.medium_language == code %}selected{% endif %}>
                        {{ lang.language|default:code }}
                    </option>
                {% endif %}
            {% endfor %}
            {% endwith %}
        </select>

        <span class="help-block">
            {_ Some images, PDFs, audio and video are presented in a single language. Here you can select this language. _}
        </span>
    </div>
{% endif %}