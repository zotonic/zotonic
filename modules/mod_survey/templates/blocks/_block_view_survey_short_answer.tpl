{% include "_survey_block_name_check.tpl" %}

<div class="control-group survey-short-answer question-{{ nr }}">
    <label for="{{ #id }}">{{ blk.prompt }}</label>
{% if blk.explanation %}
     <p class="help-block">{{ blk.explanation|linebreaksbr }}</p>
{% endif %}
    <div class="controls">
    {% if blk.validation == "date" %}
        {% include "blocks/_block_view_survey_short_answer_date.tpl" %}
    {% else %}
        <input type="text" class="{% if blk.validation == 'numericality' %}input-small{% elseif blk.validation == 'phone' %}input-medium{% else %}span6{% endif %}" name="{{ blk.name }}" id="{{ #id }}" value="{{ answers[blk.name]|escape }}" {% if blk.placeholder %}placeholder="{{ blk.placeholder|escape }}"{% endif %}/>
        {% if blk.is_required %}
            {% if blk.validation == "email" %}
                {% validate id=#id name=blk.name type={presence} type={email failure_message=_"must be an e-mail address"} %}
            {% elseif blk.validation == "numericality" %}
                {% validate id=#id name=blk.name type={presence} type={numericality not_a_number_message=_"must be a number"} %}
            {% elseif blk.validation == "phone" %}
                {% validate id=#id name=blk.name type={presence} type={format pattern="^[0-9\\(\\)\\+\\- ]+$" failure_message=_"must be a phone number"} %}
            {% else %}
                {% validate id=#id name=blk.name type={presence} %}
            {% endif %}
        {% else %}
            {% if blk.validation == "email" %}
                {% validate id=#id name=blk.name type={email failure_message=_"must be an e-mail address"} %}
            {% elseif blk.validation == "numericality" %}
                {% validate id=#id name=blk.name type={numericality not_a_number_message=_"must be a number"} %}
            {% elseif blk.validation == "phone" %}
                {% validate id=#id name=blk.name type={format pattern="^[0-9\\(\\)\\+\\- ]+$" failure_message=_"must be a phone number"} %}
            {% endif %}
        {% endif %}
    {% endif %}
    </div>
</div>
