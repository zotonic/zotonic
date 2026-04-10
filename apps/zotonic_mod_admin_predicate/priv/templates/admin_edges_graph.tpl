{% extends "admin_base_responsive.tpl" %}

{% block title %}{_ Page connections graph _}{% endblock %}

{% block head_extra %}
    {% lib "lib/css/admin-graph.css" %}
{% endblock %}

{% block content_title %}
    {% if m.rsc[q.id].id as id %}
        <h2>
            <span class="text-muted">{_ Graph overview for: _}</span>
            {{ id.title }}
            <a class="btn btn-default btn-xs"
               data-onclick-topic="model/location/post/redirect/back"
               href="{% url admin %}"
            >
                &lt; {_ Back _}
            </a>
            <a class="btn btn-default btn-xs" href="{% url admin_edit_rsc id=id %}">
                {_ Edit _}
            </a>
        </h2>

        <div class="controls pull-right">
            <button class="btn btn-default" id="step-back" title="{_ Hide previously added pages and connections. _}">↺ {_ Step back _}</button>
            <button class="btn btn-default" id="toggle-path-only" title="{_ Hide or show pages that are not on the path from the start page. _}">⏿ {_ Path _}</button>
            <button class="btn btn-default" id="filter-predicates" title="{_ Hide or show connections. _}"><i class="glyphicon glyphicon-filter"></i> {_ Filter... _}</button>
            <button class="btn btn-default" id="zoom-in">+ {_ Zoom In _}</button>
            <button class="btn btn-default" id="zoom-out">– {_ Zoom Out _}</button>
        </div>

        <p class="help-block">
            <span class="badge">{_ Beta _}</span>
            {_ This feature is in early development and may not work perfectly. _}
        </p>
    {% else %}
        <p class="alert alert-warning">
            {_ No such page or page not visible. _}
            <a class="btn btn-default btn-xs" data-onclick-topic="model/location/post/redirect/back" href="{% url admin %}">
                &lt; {_ Back _}
            </a>
        </p>
    {% endif %}
{% endblock %}


{% block content %}
    {% if m.rsc[q.id].id as id %}
        <div class="graph-container">
            <div id="graph" class="graph-viewer" aria-label="{_ Page connections graph _}"></div>
            <div id="active-resource" class="graph-active-resource" style="display: none"></div>
        </div>

        {% javascript %}
            // Set a limit for the number of edges to fetch in each request to prevent
            // overwhelming the graph with too much data at once.
            // The limit is separately applied to the list of incoming and outgoing
            // edges, so the total number of edges connected to a node could be up
            // to twice this limit.
            const EDGES_LIMIT = 300;

            cotonic.broker.call("model/sessionStorage/get/admin-graph-hidden-predicates")
                .then((m) => {
                    let hiddenPredicates = [];

                    if (Array.isArray(m.payload)) {
                        hiddenPredicates = m.payload
                            .map((predicateId) => Number(predicateId))
                            .filter((predicateId) => Number.isFinite(predicateId));
                    }
                    document.getElementById('filter-predicates').classList.toggle("is-active", hiddenPredicates.length > 0);

                    cotonic.broker
                        .call("bridge/origin/model/edge/get/graph", {
                                ids: [ {{ id }} ],
                                suppress_predicates: hiddenPredicates,
                                limit: EDGES_LIMIT,
                                unescape: true
                        }).then(
                            (m) => {
                                ResourceGraph.setGraph({
                                    nodes: m.payload.result.nodes,
                                    edges: m.payload.result.edges,
                                    startNodeId: {{ id }},
                                    hiddenPredicates: hiddenPredicates,
                                    hiddenCategories: [],
                                    setActiveToStart: true,
                                    useWorker: true
                                  });
                            }
                        )
                });

            window.addEventListener("resource:needs-edges", (e) => {
                const fromNodeId = e.detail.id;
                const hiddenPredicates = ResourceGraph.getHiddenPredicates();
                cotonic.broker
                    .call("bridge/origin/model/edge/get/graph", {
                        ids: [ fromNodeId ],
                        suppress_predicates: hiddenPredicates,
                        limit: EDGES_LIMIT,
                        unescape: true
                    }).then(
                        (m) => {
                            ResourceGraph.applyIncrementalUpdate({
                                nodes: m.payload.result.nodes,
                                edges: m.payload.result.edges,
                                nearResourceId: fromNodeId
                            });

                            if (m.payload.result.is_truncated) {
                                z_event("edges-truncated-warning");
                            }
                        }
                    );
            });

            window.addEventListener("resource:active-change", (e) => {
                let showDetails = true;
                const infoPanelDetails = document.querySelector("#active-resource details");
                if (infoPanelDetails) {
                    showDetails = infoPanelDetails.open;
                }
                z_event("show-active-resource", {
                    id: e.detail.id,
                    path: e.detail.path,
                    path_nodes: e.detail.pathNodes,
                    show_details: showDetails
                })
            });

            document.getElementById('filter-predicates').addEventListener("click", () => {
                const hiddenPredicates = ResourceGraph.getHiddenPredicates();
                z_event("filter-predicates-dialog", { hidden_predicates: hiddenPredicates });
            });
        {% endjavascript %}

        {% wire name="show-active-resource"
                action={update target="graph-active-resource-detail"
                               content="
                                    <span class='text-muted'>
                                        <img src='/img/loading.gif' height='16'>
                                        {_ Loading... _}
                                    </span>"
                }
                action={fade_in target="active-resource"}
                action={update target="active-resource" template="_admin_graph_resource.tpl"}
        %}

        {% wire name="edges-truncated-warning"
                action={alert
                    title=_"Too many connections"
                    text=_"This page has too many connections to display all at once. Only the first 300 incoming or outgoing connections are shown."
                }
        %}

        {% wire name="filter-predicates-dialog"
                action={dialog_open template="_admin_graph_filter_predicates.tpl" title=_"Filter connections"}
        %}
    {% endif %}
{% endblock %}

{% block js_extra %}
    {% comment %}
    <script src="https://cdn.jsdelivr.net/npm/graphology@0.26.0/dist/graphology.umd.min.js"></script>
    <script src="https://cdn.jsdelivr.net/npm/sigma@3.0.2/dist/sigma.min.js"></script>
    {% endcomment %}

    {% lib "lib/js/graphology.umd.min.js" %}
    {% lib "lib/js/sigma.min.js" %}
    {% lib "lib/js/admin-graph.js" %}
{% endblock %}
