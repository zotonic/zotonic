{% include "_survey_block_name_check.tpl" %}

<div class="control-group survey-short-answer question-{{ nr }}">
    <label for="{{ #id }}">{{ blk.prompt }}</label>
{% if blk.explanation %}
     <p class="help-block">{{ blk.explanation|linebreaksbr }}</p>
{% endif %}
    <div class="controls">
    {% if blk.validation == "date" %}
        <input type="hidden" class="span6" name="{{ blk.name }}" id="{{ #id }}" value="{{ answers[blk.name]|escape }}" />
        {% if blk.is_required %}
            {% validate id=#id name=blk.name type={presence} %}
        {% endif %}

        <select id="{{ #y }}" class="input-mini">
            <option></option>
            {% with now|date:"Y" as year %}
            {% for i in year|range:(year-110):(0-1) %}
                <option>{{ i }}</option>
            {% endfor %}
            {% endwith %}
        </select>
        <select id="{{ #m }}" class="input-medium">
            <option></option>
            <option value="1">{_ January _}</option>
            <option value="2">{_ February _}</option>
            <option value="3">{_ March _}</option>
            <option value="4">{_ April _}</option>
            <option value="5">{_ May _}</option>
            <option value="6">{_ June _}</option>
            <option value="7">{_ July _}</option>
            <option value="8">{_ August _}</option>
            <option value="9">{_ September _}</option>
            <option value="10">{_ October _}</option>
            <option value="11">{_ November _}</option>
            <option value="12">{_ December _}</option>
        </select>
        <select id="{{ #d }}" class="input-mini">
            <option></option>
            {% for i in 1|range:31 %}
                <option>{{ i }}</option>
            {% endfor %}
        </select>

        {% javascript %}
            var d = $('#{{ #id }}').val();
            if (d != '') {
                d = new Date(d);
                $('#{{ #y }}').val(d.getFullYear());
                $('#{{ #m }}').val(d.getMonth()+1);
                $('#{{ #d }}').val(d.getDate());
            }
            $('#{{ #y }},#{{ #m }},#{{ #d }}').change(function() {
                var y = $('#{{ #y }}').val();
                var m = $('#{{ #m }}').val();
                var d = $('#{{ #d }}').val();

                if (y && m && d) {
                    d = new Date(y, m-1, d);
                    $('#{{ #y }}').val(d.getFullYear());
                    $('#{{ #m }}').val(d.getMonth()+1);
                    $('#{{ #d }}').val(d.getDate());
                    $('#{{ #id }}').val(
                        '' + d.getFullYear()
                        + '-' + ('0'+(d.getMonth()+1)).slice(-2)
                        + '-' + ('0'+d.getDate()).slice(-2)
                    );
                } else {
                    $('#{{ #id }}').val('');
                }
            });
        {% endjavascript %}

    {% else %}
        <input type="text" class="{% if blk.validation == 'numericality' %}input-small{% elseif blk.validation == 'phone' %}input-medium{% else %}span6{% endif %}" name="{{ blk.name }}" id="{{ #id }}" value="{{ answers[blk.name]|escape }}" />
        {% if blk.is_required %}
            {% if blk.validation == "email" %}
                {% validate id=#id name=blk.name type={presence} type={email} %}
            {% elseif blk.validation == "numericality" %}
                {% validate id=#id name=blk.name type={presence} type={numericality} %}
            {% elseif blk.validation == "phone" %}
                {% validate id=#id name=blk.name type={presence} type={format pattern="^[0-9\\(\\)\\+\\- ]+$"} %}
            {% else %}
                {% validate id=#id name=blk.name type={presence} %}
            {% endif %}
        {% else %}
            {% if blk.validation == "email" %}
                {% validate id=#id name=blk.name type={email} %}
            {% elseif blk.validation == "numericality" %}
                {% validate id=#id name=blk.name type={numericality} %}
            {% elseif blk.validation == "phone" %}
                {% validate id=#id name=blk.name type={format pattern="^[0-9\\(\\)\\+\\- ]+$"} %}
            {% endif %}
        {% endif %}
    {% endif %}
    </div>
</div>
