{% if m.site.environment != `production` %}
    {% lib
        "css/prism.css"
        "js/prism.js"
    %}
{% endif %}
