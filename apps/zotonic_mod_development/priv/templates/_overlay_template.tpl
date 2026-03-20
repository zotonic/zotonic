<div class="template-debug">
    <div class="template-debug-filename">
        <tt>{{ template_file|escape }}</tt>
    </div>
    <div class="template-debug-source">
        {{ template_html }}
    </div>
    <div class="template-debug-data" id="overlay-development_debug">
        <div class="alert alert-info" style="display: none">
            {_ Maximum tracing time reached, debugging stopped. _}
            <button id="template-debug-start" class="btn btn-xs btn-primary">
                {_ Click to restart _}
            </button>
        </div>
        <div class="template-debug-data-options">
            <label class="checkbox">
                <input type="checkbox" value="all">
                {_ Debug all sessions _}
            </label>
            <p class="help-block">
                <small>
                    {_ Uncheck to debug only your current session. _}
                    {_ Debugging all sessions can result in a large amount of debug data. _}
                </small>
            </p>
        </div>
        <div id="template-debug-data">
            <!-- Debug data will be loaded here -->
            <p class="help-block">
                {_ Check debug points in the template source to see debug data here. _}
            </p>
        </div>
    </div>
</div>

{% wire name="template_debug_enable"
        postback={template_debug_enable template=template_file}
        delegate=`mod_development`
%}

{% javascript %}
    function debug_start() {
        const is_all = $(".template-debug-data input[value='all']").is(":checked");
        const checkboxes = $(".template-debug-source input:checked");
        let enabled = [];

        checkboxes.each(function() {
            enabled.push($(this).attr("value"));
        });

        $('#overlay-development_debug > .alert').fadeIn();
        if (enabled.length === 0) {
            $("#template-debug-data").html('<p class="help-block">{_ Check debug points in the template source to see debug data here. _}</p>');
        } else {
            $("#template-debug-data").html('<p class="help-block">{_ Waiting for debug data... _}</p>');
        }

        z_event("template_debug_enable", {
            enabled: enabled,
            is_all: is_all
        });
    }

    $(".template-debug input").on('input', debug_start);
    $("#template-debug-start").on('click', debug_start);

    $(".modal-overlay-close").on('click', () => {
        const checkboxes = $(".template-debug-source input:checked");
        if (checkboxes.length > 0) {
            z_event("template_debug_enable", {
                enabled: [],
                is_all: false
            });
        }
    });

    $('#template-debug-data').on("mouseenter mouseleave", "details", function(e) {
        const line = $(this).data("line");
        $(".template-compiler-line").removeClass("highlighted");
        if (e.type === "mouseenter") {
            $(`.template-compiler-line[data-line="${line}"]`).addClass("highlighted");
        }
    });

    cotonic.broker.subscribe(
        "bridge/origin/model/development/event/template/debug",
        (msg) => {
            const payload = msg.payload;
            switch (payload.event) {
                case "data":
                    if (payload.filename == '{{ template_file|escapejs }}') {
                        const id = `template-data-${payload.line}-${payload.column}`;
                        let html;

                        if ($('#' + id).length == 0) {
                            html = `<details id="${id}" data-line="${payload.line}">
                                <summary>Line ${payload.line}:${payload.column}</summary>
                            </details>`;
                            $('#template-debug-data').append(html);
                            $('#template-debug-data > .help-block').remove();
                        }
                        const table = $('<table class="table table-striped"></table>');
                        const tbody = $('<tbody></tbody>');
                        for (const [key, value] of Object.entries(payload.data)) {
                            const tr = $('<tr></tr>');
                            const th = $('<th></th>');
                            const td = $('<td></td>');
                            const pre = $('<pre></pre>');

                            th.text(key);
                            pre.text(JSON.stringify(value, null, 2));
                            td.append(pre);
                            tr.append(th, td);
                            tbody.append(tr);
                        }
                        table.append(tbody);
                        $("#" + id).append(table);
                    }
                    break;
                case "stop":
                    $('#overlay-development_debug > .alert').fadeIn();
                    break;
                case "start":
                    $('#overlay-development_debug > .alert').hide();
                    break;
                default:
                    break;
            }
        });
{% endjavascript %}
