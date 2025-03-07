{% include "_survey_block_name_check.tpl" %}

{% if is_survey_answer_view %}
    <div class="form-group survey-short-answer">
        <label class="control-label">{{ blk.prompt }}</label>
        {% if blk.explanation %}
            <p class="help-block">{{ blk.explanation|linebreaksbr }}</p>
        {% endif %}
        <blockquote>{{ result.answers[blk.name].answer|escape }}</blockquote>
    </div>
{% else %}
<div class="form-group survey-short-answer question-{{ nr }} {% if not blk.prompt %}noprompt{% endif %}">
    <label class="control-label" for="{{ #id }}">{{ blk.prompt }}</label>
{% if blk.explanation %}
     <p class="help-block">{{ blk.explanation|linebreaksbr }}</p>
{% endif %}
    <div style="{% if blk.validation == 'numericality' %}width: 20%{% elseif blk.validation == 'phone' %}width: 60%{% endif %}">
    {% if blk.validation == "date" %}
        {% include "blocks/_block_view_survey_short_answer_date.tpl" %}
    {% else %}
        <input  type="{% if blk.validation == 'email' %}email{% else %}text{% endif %}"
                class="form-control {% if blk.validation == 'numericality' %}input-small{% elseif blk.validation == 'phone' %}input-medium{% endif %}"
                name="{{ blk.name }}"
                id="{{ #id }}"
                value="{% if answers[blk.name]|is_defined %}{{ answers[blk.name]|escape }}{% elseif blk.validation == 'phone' %}{{ answer_user_id.phone }}{% elseif blk.validation == 'email' %}{{ answer_user_id.email }}{% elseif blk.name == 'name' %}{{ answer_user_id.name_first }} {% if answer_user_id.name_surname_prefix %}{{ answer_user_id.name_surname_prefix }} {% endif %}{{ answer_user_id.name_surname }}{% elseif blk.name|member:['phone','phone_mobile','email','name_first','name_surname','name_surname_prefix','address_street_1','address_street_2','address_city','address_state','address_postcode'] %}{{ answer_user_id[blk.name] }}{% endif %}"
                {% if blk.placeholder %}placeholder="{{ blk.placeholder|escape }}"{% endif %}
                {% if blk.validation == 'numericality' %}
                    {% if blk.minval != '' %}min="{{ blk.minval }}"{% endif %}
                    {% if blk.maxval != '' %}max="{{ blk.maxval }}"{% endif %}
                {% endif %}
        >
        {% if blk.is_required %}
            {% if blk.validation == "email" %}
                {% validate id=#id name=blk.name type={presence} type={email failure_message=_"must be an e-mail address"} %}
            {% elseif blk.validation == "numericality" %}
                {% validate id=#id name=blk.name
                            type={presence}
                            type={numericality
                                    minimum=blk.minval|to_integer
                                    maximum=blk.maxval|to_integer
                                    not_a_number_message=_"must be a number"
                            }
                %}
            {% elseif blk.validation == "phone" %}
                {% validate id=#id name=blk.name type={presence} type={format pattern="^[0-9\\(\\)\\+\\- ]+$" failure_message=_"must be a phone number"} %}
            {% else %}
                {% validate id=#id name=blk.name type={presence} %}
            {% endif %}
        {% else %}
            {% if blk.validation == "email" %}
                {% validate id=#id name=blk.name type={email failure_message=_"must be an e-mail address"} %}
            {% elseif blk.validation == "numericality" %}
                {% validate id=#id name=blk.name
                            type={numericality
                                    minimum=blk.minval|to_integer
                                    maximum=blk.maxval|to_integer
                                    not_a_number_message=_"must be a number"
                            }
                %}
            {% elseif blk.validation == "phone" %}
                {% validate id=#id name=blk.name type={format pattern="^[0-9\\(\\)\\+\\- ]+$" failure_message=_"must be a phone number"} %}
            {% endif %}
        {% endif %}
    {% endif %}
    </div>
</div>
{% endif %}
