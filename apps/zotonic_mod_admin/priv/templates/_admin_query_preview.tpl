<h4>{_ Query results _}</h4>

<div class="rows">
    <div class="col-lg-6">
        <p>
            {% if result.is_total_estimated %}{% trans "About {n} items found." n=result.total|round_significant:2 %}
            {% else %}{% trans "{n} items found." n=result.total %}
            {% endif %}
        </p>

        <ul class="tree-list connections-list">
        {% for id in result %}
            <li class="menu-item">
                {% with forloop.counter as index %}
                <div class="menu-wrapper">
                    <a id="{{ #qres.index }}" href="{% url admin_edit_rsc id=id %}" title="{_ Edit _}">
                        {% catinclude "_rsc_edge_item.tpl" id %}
                    </a>
                    {% wire id=#qres.index
                            action={dialog_edit_basics
                                id=id
                                update_element=#qres.index
                                template="_rsc_edge_item.tpl"
                                is_update
                            }
                    %}
                </div>
                {% endwith %}
            </li>
        {% empty %}
            <li><span class="text-muted">{_ No results. _}</span></li>
        {% endfor %}
        </ul>
    </div>
    <div class="col-lg-6">
        <details>
            <summary>{_ Show query as JSON _}</summary>
            <pre><code id="{{ #json }}">{{ result.search_args.q|to_json|escape }}</code></pre>
        </details>

        {% javascript %}
        {
            const json = $('#{{ #json }}').text();
            if (json) {
                const elt = $('#{{ #json }}')
                    .text(JSON.stringify(JSON.parse(json), null, 2))
                    .addClass("language-json")
                    .get(0);
                Prism?.highlightElement(elt);
            }
        }
        {% endjavascript %}
    </div>
</div>
