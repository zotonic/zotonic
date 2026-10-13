{% if m.acl.use.mod_import_feed %}
    <li id="{{ #connection }}" class="well well-sm" data-feed-connection="{{ predicate|escape }}-{{ object|escape }}">
        <input type="hidden" name="connection" value="{{ predicate|escape }}={{ object|escape }}">
        {% button class="btn btn-default btn-xs pull-right" text=_"Remove" action={remove target=#connection} %}
        <strong>{{ m.rsc[predicate].title }}</strong>
        <span aria-hidden="true"> → </span>
        <a href="{% url admin_edit_rsc id=object %}" target="_blank" rel="noopener noreferrer">{{ m.rsc[object].title|default:object }}</a>
    </li>
    {% javascript %}
        // Keep a single row when the same connection is selected again.
        (function() {
            const row = document.getElementById("{{ #connection }}");
            const siblings = row.parentElement.querySelectorAll("[data-feed-connection]");
            if (Array.from(siblings).some(other => other !== row && other.dataset.feedConnection === row.dataset.feedConnection)) {
                row.remove();
            }
        }());
    {% endjavascript %}
{% endif %}
