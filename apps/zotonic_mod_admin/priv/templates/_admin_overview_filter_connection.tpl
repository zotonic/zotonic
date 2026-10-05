{# Shared resource/predicate picker for overview filters and bulk connections. #}
{% with m.rsc[value].id, #callback|replace:"-":"_" as selected_id, callback %}
<div class="admin-filter-connection form-group">
    <label class="control-label col-md-3">{{ label|escape }}</label>
    <div class="col-md-9">
        <label class="sr-only" for="{{ #predicate }}">{_ Predicate _}</label>
        <select id="{{ #predicate }}" name="{{ predicate_name|escape }}" class="form-control">
            <option value="">{{ predicate_placeholder|default:_"Any predicate"|escape }}</option>
            {% for title, id in predicates %}
                <option value="{{ id }}" {% if id == predicate_value %}selected{% endif %}>{{ title }}</option>
            {% endfor %}
        </select>
        <div id="{{ #picker }}"{% if require_predicate and not predicate_value %} class="hidden"{% endif %}>
            <input type="hidden" id="{{ #value }}" name="{{ name|escape }}" value="{{ selected_id }}">
            <label class="sr-only" for="{{ #title }}">{{ page_label|escape }}</label>
            <div class="input-group">
                <input id="{{ #title }}" class="form-control" type="text" readonly
                    placeholder="{{ placeholder|escape }}"
                    value="{% if selected_id.is_visible %}{{ selected_id.title|default:_"Untitled"|striptags|escape }} ({{ selected_id }}){% endif %}">
                <span class="input-group-btn">
                    {% button id=#choose class="btn btn-default"
                        text=_"Select"
                        title=page_label
                        action={dialog_open
                            title=label
                            template="_action_dialog_connect.tpl"
                            intent="select"
                            tabs_enabled=["find"]
                            callback=["window.", callback]
                            autoclose
                            level=1
                        }
                    %}
                    <button type="button" id="{{ #clear }}" class="btn btn-default" title="{_ Clear _}" aria-label="{_ Clear _}">
                        <span aria-hidden="true">&times;</span>
                    </button>
                </span>
            </div>
        </div>
    </div>
</div>
{% javascript %}
    window["{{ callback }}"] = function(resource) {
        document.getElementById("{{ #value }}").value = resource.object_id;
        // Titles returned by the resource model are HTML-escaped; decode as text.
        const title = document.createElement("textarea");
        title.innerHTML = resource.title || "";
        document.getElementById("{{ #title }}").value = title.value + " (" + resource.object_id + ")";
    };
    document.getElementById("{{ #clear }}").addEventListener("click", function() {
        document.getElementById("{{ #value }}").value = "";
        document.getElementById("{{ #title }}").value = "";
    });
    {% if require_predicate %}
        (function() {
            const predicate = document.getElementById("{{ #predicate }}");
            const picker = document.getElementById("{{ #picker }}");
            const updatePicker = function() {
                const hasPredicate = predicate.value !== "";
                picker.classList.toggle("hidden", !hasPredicate);
                if (!hasPredicate) {
                    document.getElementById("{{ #value }}").value = "";
                    document.getElementById("{{ #title }}").value = "";
                }
            };
            predicate.addEventListener("change", updatePicker);
            updatePicker();
        }());
    {% endif %}
{% endjavascript %}
{% endwith %}
