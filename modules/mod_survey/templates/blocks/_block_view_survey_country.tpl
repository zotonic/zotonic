{% include "_survey_block_name_check.tpl" %}
<div class="control-group survey-country question-{{ nr }} {% if not blk.prompt %}noprompt{% endif %}">
    <label class="control-label" for="{{ #id }}">{{ blk.prompt }}</label>
{% if blk.explanation %}
     <p class="help-block">{{ blk.explanation|linebreaksbr }}</p>
{% endif %}
    <div class="controls">
        <select name="{{ blk.name }}" id="{{ #id }}">
            <option value="">{_ Select country _}</option>
            <option disabled></option>
            {% include "_l10n_country_options.tpl" country=answers[blk.name]|default_if_none:m.acl.user.address_country %}
        </select>
    </div>
</div>
{% if blk.is_required %}
    {% validate id=#id name=blk.name type={presence} %}
{% endif %}
