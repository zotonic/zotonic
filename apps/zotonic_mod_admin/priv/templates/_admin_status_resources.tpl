<div class="widget">
    <div class="widget-header">{_ Erlang memory _} <small>{_ All sites on this node _}</small></div>
    <div class="widget-content">
        {% with m.admin_status.memory.used, m.admin_status.memory.allocated as used, total %}
            <div class="admin-capacity-summary">
                <strong>{{ used|filesizeformat }} {_ used _}</strong>
                <span>{{ total|filesizeformat }} {_ allocated _}</span>
            </div>
            {% include "_admin_status_capacity.tpl" label=_"Erlang memory in use" used=used total=total %}
            <p class="help-block">{{ (total - used)|filesizeformat }} {_ unused allocation, available for reuse by Erlang. This is not free host memory. _}</p>
        {% endwith %}
    </div>
</div>

<div id="os_memory_status" class="widget">
    <div class="widget-header">{_ OS memory _} <small>{_ Host _}</small></div>
    <div class="widget-content">
        {% with m.admin_status.os_memory as os %}
            {% if os.total_memory > 0 %}
                {% ifnotequal os.available_memory `undefined` %}
                    <div class="admin-capacity-summary">
                        <strong>{{ os.available_memory|filesizeformat }} {_ available _}</strong>
                        <span>{{ os.total_memory|filesizeformat }} {_ total _}</span>
                    </div>
                    {% include "_admin_status_capacity.tpl" label=_"OS memory unavailable" used=(os.total_memory - os.available_memory) total=os.total_memory alert=m.admin_status.os_memory.alert %}
                    <p class="help-block">{_ The bar shows memory not available for allocation. Available memory includes reclaimable memory reported by the operating system. _}</p>
                {% else %}
                    <div class="admin-capacity-summary">
                        <strong>{{ (os.total_memory - os.free_memory)|filesizeformat }} {_ used _}</strong>
                        <span>{{ os.total_memory|filesizeformat }} {_ total _}</span>
                    </div>
                    {% include "_admin_status_capacity.tpl" label=_"OS memory in use" used=(os.total_memory - os.free_memory) total=os.total_memory alert=m.admin_status.os_memory.alert %}
                    <p class="help-block">{{ os.free_memory|filesizeformat }} {_ free. Used memory can include caches; high usage alone does not indicate memory pressure. _}</p>
                {% endifnotequal %}
            {% else %}
                <p>{_ Memory information unavailable. _}</p>
            {% endif %}
            {% ifnotequal os.total_swap `undefined` %}
                <div class="admin-capacity-summary">
                    <strong>{_ Swap _}</strong>
                    {% if os.total_swap > 0 %}<span>{{ (os.total_swap - os.free_swap)|filesizeformat }} {_ used _} / {{ os.total_swap|filesizeformat }} {_ total _}</span>{% endif %}
                </div>
                {% if os.total_swap > 0 %}
                    {% include "_admin_status_capacity.tpl" label=_"Swap in use" used=(os.total_swap - os.free_swap) total=os.total_swap %}
                    <p class="help-block">{{ os.free_swap|filesizeformat }} {_ free _}</p>
                {% else %}
                    <p class="text-muted">{_ Disabled _}</p>
                {% endif %}
            {% endifnotequal %}
        {% endwith %}
        <details class="admin-capacity-details">
            <summary>{_ Detailed memory figures _}</summary>
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
        </details>
    </div>
</div>

<div id="disk_status" class="widget">
    <div class="widget-header">{_ Disks _} <small>{_ Host _}</small></div>
    <div class="widget-content">
        <div class="admin-capacity-disks">
            {% for d in m.admin_status.disks %}
                <div class="admin-capacity-disk">
                    <div class="admin-capacity-summary">
                        <strong>{{ d.disk|escape }}</strong>
                        <span class="text-muted">{{ (d.size * 1024)|filesizeformat }} {_ capacity _} / <small>{{ (d.size * 1024 * (100 - d.percent_used) / 100)|round|filesizeformat }} {% trans "free" msgctxt="disk space" %}</small></span>
                        {% if d.percent_used > 95 %}
                            <span class="label label-danger">{_ Less than 5% free disk space _}</span>
                        {% endif %}
                    </div>
                    {% include "_admin_status_capacity.tpl" label=d.disk used=d.percent_used total=100 alert=d.alert %}
                </div>
            {% empty %}
                <p>{_ Disk information unavailable. _}</p>
            {% endfor %}
        </div>
        <p class="help-block">{_ Bars show disk space in use. Refreshed every 30 minutes. _}</p>
    </div>
</div>
