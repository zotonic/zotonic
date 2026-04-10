<p>{_ Only show connections with the following predicates: _}</p>

<ul class="list-unstyled" id="predicates-filter-list">
    {% for id in m.search.query::%{ cat: "predicate" }|sort:`title` %}
        <li>
            <label class="checkbox">
                <input type="checkbox" value="{{ id }}" {% if not id|member:q.hidden_predicates %}checked{% endif %}>
                {{ id.title }}
                <small class="text-muted">/ {{ id.name }}</small>
            </label>
        </li>
    {% endfor %}
</ul>

<p class="help-block">
    {_ If a resource has many incoming or outgoing connections, then the unchecked predicates might be omitted from the graph data. _}
</p>

<div class="modal-footer">
    {% button class="btn btn-primary" text=_"Close" action={dialog_close} %}
</div>

{% javascript %}
    document.getElementById("predicates-filter-list").addEventListener("input", (e) => {
        if (e.target.tagName === "INPUT") {
            const checkboxes = document.querySelectorAll("#predicates-filter-list input[type=checkbox]");
            const hiddenPredicates = [];
            checkboxes.forEach((checkbox) => {
                if (!checkbox.checked) {
                    hiddenPredicates.push(checkbox.value);
                }
            });
            ResourceGraph.setHiddenPredicates(hiddenPredicates);
            document.getElementById('filter-predicates').classList.toggle("is-active", hiddenPredicates.length > 0);
            cotonic.broker.publish("model/sessionStorage/post/admin-graph-hidden-predicates", hiddenPredicates);
        }
    });
{% endjavascript %}
