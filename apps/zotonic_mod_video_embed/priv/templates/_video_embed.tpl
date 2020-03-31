{% if is_iframe and medium.width and medium.height %}
    <div class="embed-responsive" style="padding-top: {{ medium.height / medium.width * 100 }}%">
        {{ html }}
    </div>
{% else %}
    {{ html }}
{% endif %}
