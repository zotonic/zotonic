{% include "_survey_block_name_check.tpl" %}
{% if is_survey_answer_view %}
    <div class="control-group survey-country">
        <label class="control-label">{{ blk.prompt }}</label>
        {% with result.answers[blk.name].answer as ans %}
        {% with m.l10n.countries as countries %}
            <ul>
            {% for iso_code,name in countries %}
                {% if iso_code|member:ans %}<li>{{ name }}</li>{% endif %}
            {% endfor %}
            </ul>
        {% endwith %}
        {% endwith %}
    </div>
{% else %}
    <div class="form-group survey-country question-{{ nr }} {% if not blk.prompt %}noprompt{% endif %}">
        <label class="control-label" for="{{ #id }}">{{ blk.prompt }}</label>
        {% if blk.explanation %}
             <p class="help-block">{{ blk.explanation|linebreaksbr }}</p>
        {% endif %}
        <div>
            <select class="form-control" name="{{ blk.name }}" id="{{ #id }}">
                <option value="">{_ Select country _}</option>
                <option disabled></option>
                {% include "_l10n_country_options.tpl" country=answers[blk.name]|default_if_none:m.acl.user.address_country %}
            </select>
        </div>
    </div>
    {% if blk.is_required %}
        {% validate id=#id name=blk.name type={presence} %}
    {% endif %}
{% endif %}
