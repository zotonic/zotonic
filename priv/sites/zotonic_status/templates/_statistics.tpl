<script>window.graphs={};</script>

{% for node, h, resps in stats %}

<div class="zp-100">
    <h2>{{ node }}</h2>
</div>

<table>
    {% for respa, respb in resps|chunk:2 %}
    <tr>
        <td>
            {% with respa[1], respa[2] as k, stats %}
            <h3>{{ k|statsname }}</h3>
            
            <div class="graph" id="graph-{{ k | statskey }}"></div>
            <script>
                window.graphs["{{ k | statskey }}"] = $.plot(
                                        $("#graph-{{ k | statskey }}"), 
                                        [{{stats.second|statsarray}}],
                                        {
                                            yaxis: {
                                                min: 0,
                                                max: {{ stats|statsmax }}
                                            },
                                            grid: {
                                                markings: [
                                                    {% if stats.avg %}
                                                    { color: '#33c', lineWidth: 2, yaxis: { from: {{stats.avg}}, to: {{stats.avg}} } },
                                                    {% endif %}
                                                    {% if stats.perc95 %}
                                                    { color: '#c33', lineWidth: 2, yaxis: { from: {{stats.perc95}}, to: {{stats.perc95}} } }
                                                    {% endif %}
                                                ]
                                            }
                                        });
            </script>

            {% if stats.perc99 %}
            <p class="meta"id="stats-{{ k|statskey }}">
                {% include "_statsline.tpl" k=k stats=stats %}
            </p>
            {% endif %}
            {% endwith %}
        </td>


        <td>
            {% with respb[1], respb[2] as k, stats %}
            <h3>{{ k|statsname }}</h3>
            
            <div class="graph" id="graph-{{ k | statskey }}"></div>
            <script>
            window.graphs["{{ k | statskey }}"] = $.plot(
                                    $("#graph-{{ k | statskey }}"), 
                                    [{{stats.second|statsarray}}],
                                    {
                                        yaxis: {
                                            min: 0,
                                            max: {{ stats|statsmax }}
                                        },
                                        grid: {
                                            markings: [
                                                {% if stats.avg %}
                                                { color: '#33c', lineWidth: 2, yaxis: { from: {{stats.avg}}, to: {{stats.avg}} } },
                                                {% endif %}
                                                {% if stats.perc95 %}
                                                { color: '#c33', lineWidth: 2, yaxis: { from: {{stats.perc95}}, to: {{stats.perc95}} } }
                                                {% endif %}
                                            ]
                                        }
                                    });
            </script>

            {% if stats.perc99 %}
            <p class="meta" id="stats-{{ k|statskey }}">
                {% include "_statsline.tpl" k=k stats=stats %}
            </p>
            {% endif %}
            {% endwith %}
        </td>


    </tr>
    {% endfor %}
</table>


{% endfor %}


