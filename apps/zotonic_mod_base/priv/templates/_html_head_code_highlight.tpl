{% if m.site.environment != `production` and not no_prism %}
    {% lib
        "css/prism.css"
        "js/prism.js"
    %}
{% endif %}
