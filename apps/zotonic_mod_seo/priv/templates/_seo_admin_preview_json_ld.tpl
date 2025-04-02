{% if m.acl.anonymous.view[id] %}
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
{% else %}
    <div class="text-muted well">
        {_ This page is not publicly viewable. JSON-LD is only generated if the page is visible for anonymous visitors. _}
    </div>
{% endif %}
