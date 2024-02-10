{% if is_iframe and medium.width and medium.height %}
    <div class="embed-responsive">
        {{ html }}
    </div>
{% else %}
    {{ html }}
{% endif %}
