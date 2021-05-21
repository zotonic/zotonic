
<p>Loaded #{{ n|escape }}</p>

{% if n < till %}
    <script>
        document.location = "/tests/initial_postback_test?n={{ n }}";
    </script>
{% else %}
    <p>Success!</p>
{% endif %}
