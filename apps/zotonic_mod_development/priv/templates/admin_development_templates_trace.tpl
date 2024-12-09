{% extends "admin_base.tpl" %}

{% block title %} Development {% endblock %}

{% block head_extra %}
    {% inherit %}
    <style type="text/css" nonce="{{ m.req.csp_nonce }}">
        #trace-status span {
            display: none;
        }
        .trace-stopped #trace-status span.trace-stopped {
            display: inline-block;
        }
        .trace-stopped .trace-loading {
            visibility: hidden;
        }
        .trace-session #trace-status span.trace-session {
            display: inline-block;
        }
        .trace-all #trace-status span.trace-all {
            display: inline-block;
        }
        .trace-other #trace-status span.trace-other {
            display: inline-block;
        }
    </style>
{% endblock %}

{% block content %}
<ul class="breadcrumb">
    <li><a href="{% url admin_development %}">{_ Site Development _}</a></li>
    <li class="active">{_ Live traces of template rendering _}</li>
</ul>

<div class="admin-header">
    <h2>{_ Live traces of template rendering _}</h2>
    <p>{_ Enable tracing of all template renderings. _}<br>{_ A graph of all templates, and by whom they are included, is generated and automatically updated. _}</p>
    <p>{_ Traces automatically stop if this page is not visited during more than 10 minutes. _}</p>
</div>

<div class="well">
    {% button class="btn btn-primary" text=_"Clear data and start"
              postback={template_trace_start}
              delegate=`mod_development`
    %}
    {% button class="btn btn-primary" text=_"Clear data and trace all"
              postback={template_trace_start all}
              delegate=`mod_development`
    %}
    {% button class="btn btn-default" text=_"Stop"
              postback={template_trace_stop}
              delegate=`mod_development`
    %}

    <span id="trace-status" class="">
        <span class="text-muted trace-session">
            <img src="/lib/images/spinner.gif" height="16" width="16"> {_ Tracing this session... _}
        </span>
        <span class="text-muted trace-other">
            <img src="/lib/images/spinner.gif" height="16" width="16"> {_ Tracing other session... _}
        </span>
        <span class="text-muted trace-all">
            <img src="/lib/images/spinner.gif" height="16" width="16"> {_ Tracing all sessions... _}
        </span>
        <span class="text-muted trace-stopped">
            {_ Stopped. _}
        </span>
    </span>
</div>

<div class="widget">
    <div class="widget-header">
        {_ Graph of all template dependencies _}
    </div>
    <div class="widget-content" id="xref-results">
        <button id="graphviz_copy" class="btn btn-default pull-right">{_ Copy GraphViz data _}</button>
        <p class="help-block">
            {_ Dependency graph of all traced templates. All includes are shown. _}<br>
            {% trans "<tt>{extends}</tt> are shown with a striped line, <tt>{overrules}</tt> are shown with a dotted line."
                extends="{% extends \"...\" %}"
                overrules="{% overrules %}"
            %}
        </p>

        <div id="graphviz_svg" style="max-width: 100%; overflow: auto;">
            <p style="text-muted" class="trace-loading">
                <img src="/lib/images/spinner.gif" height="16" width="16">
                {_ Loading... _}
            </p>
        </div>

        <div style="display: none">
            <textarea class="form-control" id="graphviz_data" disabled></textarea>
            <textarea class="form-control" id="graphviz_data_prev" disabled></textarea>
        </div>

        {% wire name="update-dot"
                postback={template_trace_fetch textarea="graphviz_data" status="trace-status"}
                delegate=`mod_development`
        %}

        {% javascript %}
            $('#graphviz_data').on('change', function() {
                const vizData = $('#graphviz_data').val();
                const vizDataPrev = $('#graphviz_data_prev').val();
                if (vizData) {
                    if (vizData != vizDataPrev) {
                        var svg = Viz(vizData, "svg");
                        $('#graphviz_svg').html(svg);
                        $('#graphviz_svg title').remove();
                        $('#graphviz_data_prev').val(vizData);
                    }
                } else {
                    $('#graphviz_svg').html('<p class="text-muted">{_ No data. _}</p>');
                }
            });

            $('#graphviz_copy').on('click', function() {
                const text = $('#graphviz_data').val();
                navigator.clipboard.writeText(text).then(
                    function() {
                        z_growl_add("{_ Copied dot file to the clipboard. _}");
                    },
                    function(err) {
                        z_growl_add("{_ Could not copy dot file to the clipboard. _}", false, 'error');
                    });
            });

            setInterval(() => z_event("update-dot", {}), 1000);
            z_event("update-dot", {});
        {% endjavascript %}
    </div>
</div>

{% endblock %}

{% block js_extra %}
    {% lib
        "js/viz.js"
    %}
{% endblock %}
