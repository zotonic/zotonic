<div class="widget">
    <div class="widget-header">{_ Erlang memory _} <small>{_ All sites on this node _}</small></div>
    <div class="widget-content">
        <div class="table-responsive"><table class="table table-sm">
            <thead>
                <tr>
                    <th>{_ Allocated _}</th>
                    <th>{_ Used _}</th>
                    <th>{_ Unused _}</th>
                    <th>{_ Percent in use _}</th>
                </tr>
            </thead>
            <tbody>
                <tr>
                    <td>{{ m.admin_status.memory.allocated | filesizeformat }}</td>
                    <td>{{ m.admin_status.memory.used | filesizeformat }}</td>
                    <td>{{ m.admin_status.memory.unused | filesizeformat }}</td>
                    <td>{{ (m.admin_status.memory.usage * 100) | round }}%</td>
                </tr>
            </tbody>
        </table></div>
    </div>
</div>

<div id="os_memory_status" class="widget">
    <div class="widget-header">{_ OS memory _} <small>{_ Host _}</small></div>
    <div class="widget-content">
        <div class="table-responsive"><table class="table table-sm">
            {% with m.admin_status.os_memory as os %}
            <thead>
                <tr>
                    <th></th>
                    <th>{_ Total _}</th>
                    <th>{_ Used _}</th>
                    <th>{_ Free _}</th>
                    {% ifnotequal os.buffered_memory `undefined` %}<th>{_ Buffered _}</th>{% endifnotequal %}
                    {% ifnotequal os.cached_memory `undefined` %}<th>{_ Cached _}</th>{% endifnotequal %}
                    {% ifnotequal os.available_memory `undefined` %}<th>{_ Available _}</th>{% endifnotequal %}
                </tr>
            </thead>
            <tbody>
                <tr>
                    <th>{_ Memory _}</th>
                    <td>{{ os.total_memory | filesizeformat }}</td>
                    <td>{{ (os.total_memory - os.free_memory) | filesizeformat }}</td>
                    <td>{{ os.free_memory| filesizeformat }}</td>
                    {% ifnotequal os.buffered_memory `undefined` %}<td>{{ os.buffered_memory | filesizeformat }}</td>{% endifnotequal %}
                    {% ifnotequal os.cached_memory `undefined` %}<td>{{ os.cached_memory | filesizeformat }}</td>{% endifnotequal %}
                    {% ifnotequal os.available_memory `undefined` %}<td>{{ os.available_memory | filesizeformat }}</td>{% endifnotequal %}
                </tr>
                {% ifnotequal os.total_swap `undefined` %}
                    <tr>
                        <th>{_ Swap _}</th>
                        <td>{{ os.total_swap | filesizeformat }}</td>
                        <td>{{ (os.total_swap - os.free_swap ) | filesizeformat }}</td>
                        <td>{{ os.free_swap | filesizeformat }}</td>
                        {% ifnotequal os.buffered_memory `undefined` %}<td></td>{% endifnotequal %}
                        {% ifnotequal os.cached_memory `undefined` %}<td></td>{% endifnotequal %}
                        {% ifnotequal os.available_memory `undefined` %}<td></td>{% endifnotequal %}
                    </tr>
                {% endifnotequal %}
            </tbody>
            {% endwith %}
        </table></div>
    </div>
</div>

<div id="disk_status" class="widget">
    <div class="widget-header">{_ Disks _} <small>{_ Host _}</small></div>
    <div class="widget-content">
        <div {% if m.admin_status.disks.alert %}class="border-danger"{% endif %}>
            <div class="table-responsive"><table class="table table-sm">
                <thead>
                    <tr>
                        <th>{_ Path _}</th>
                        <th>{_ Disk capacity _}</th>
                        <th>{_ Usage _}</th>
                    </tr>
                </thead>
                <tbody>
                    {% for d in m.admin_status.disks%}
                        <tr {% if d.alert %}style="font-weight: bold; font-size:1.1em;" class="text-danger"{% endif %}>
                            <td>{{ d.disk|escape }}</td>
                            <td>{{ (d.size * 1024)|filesizeformat }}</td>
                            <td>
                                {% if d.alert %}
                                    <span class="fa fa-warning"></span>
                                {% endif %}
                                {{ d.percent_used }}%</td>
                        </tr>
                    {% endfor %}
                </tbody>
            </table></div>
            <p class="help-block"><span class="fa fa-info-circle"></span> {_ Refreshed every 30 minutes. _}</p>
        </div>
    </div>
</div>

