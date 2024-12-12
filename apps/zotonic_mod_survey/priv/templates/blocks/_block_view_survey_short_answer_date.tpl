<div class="short-answer-date">
    {% if blk.is_required %}
        {% validate id=#id name=blk.name type={presence} %}
    {% endif %}

    <input id="{{ #id }}" type="date" name="{{ blk.name }}" value="{{ answers[blk.name]|escape }}"
        {% if blk.minval %}min="{{ blk.minval|date:'Y-m-d' }}"{% endif %}
        {% if blk.maxval %}max="{{ blk.maxval|date:'Y-m-d' }}"{% endif %}
    >
</div>
