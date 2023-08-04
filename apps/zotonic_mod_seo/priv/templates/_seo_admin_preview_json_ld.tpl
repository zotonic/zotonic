{% with lang_code as z_language %}
    <pre><code id="{{ #jsonld }}" class="language-json">{{ m.seo.jsonld[id]|escape }}</code></pre>
    {% javascript %}
    {
        let jsonld = $('#{{ #jsonld }}').text();
        if (jsonld) {
            let elt = $('#{{ #jsonld }}')
                .text(JSON.stringify(JSON.parse(jsonld), null, 2))
                .addClass("language-json")
                .get(0);
            Prism.highlightElement(elt);
        }
    }
    {% endjavascript %}
{% endwith %}
