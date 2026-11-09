<section class="widget analytics-status">
    <h3 class="widget-header">{_ Analytics status _}</h3>
    <div class="widget-content">
        <div class="analytics-status-heading">
            {% if a.status.running %}
                <strong class="analytics-status-label">● {_ Analytics is running _}</strong>
            {% else %}
                <strong class="text-warning">{_ Analytics is unavailable _}</strong>
            {% endif %}
            <span class="text-muted">{_ Snapshot at page load. Refresh to update. _}</span>
        </div>
        {% if not a.status.running %}<p class="alert alert-warning">{_ New events cannot currently be processed. Previously saved history is still available. _}</p>{% endif %}
        {% if a.status.failures %}<p class="alert alert-warning">{_ Database writes are being retried. Event processing can continue while buffer capacity remains available; prolonged failures can cause data loss. _}</p>{% endif %}

        <div class="analytics-capacity-grid">
            <div class="analytics-capacity">
                <label for="analytics-storage">{_ Reserved storage capacity _}</label>
                <div class="analytics-capacity-value"><strong>{{ a.usage.reserved }}</strong> / {{ a.usage.budget }} {_ group/time slots _}</div>
                <meter id="analytics-storage" min="0" max="{{ a.usage.budget }}" value="{{ a.usage.reserved }}"></meter>
                <p>{_ Capacity is reserved for the full history of every group in a recording. This is a planning limit, not disk usage. Pausing keeps the reservation; deleting a recording releases it. _}</p>
            </div>
            <div class="analytics-capacity">
                <label for="analytics-recording-capacity">{_ Recording capacity _}</label>
                <div class="analytics-capacity-value"><strong>{{ a.usage.recordings }}</strong> / 32 {_ recordings _}</div>
                <meter id="analytics-recording-capacity" min="0" max="32" value="{{ a.usage.recordings }}"></meter>
                <p>{_ Active and paused recordings both count towards this limit. A new recording also needs enough free storage capacity for its group limit. _}</p>
            </div>
            <div class="analytics-capacity">
                <label for="analytics-buffer">{_ Event buffer _}</label>
                {% if a.status.running %}
                    <div class="analytics-capacity-value"><strong>{{ a.status.entries }}</strong> / {{ a.status.entry_limit }} {_ counters _}</div>
                    <meter id="analytics-buffer" min="0" max="{{ a.status.entry_limit }}" value="{{ a.status.entries }}"></meter>
                    <p>{_ Events are combined into counters in memory before saving, normally every 10 seconds. A low value is normal. This buffer is separate from a batch currently being saved. _}</p>
                    {% if a.status.pending %}<p><strong>{_ A batch is waiting to be saved. _}</strong></p>{% endif %}
                {% else %}
                    <p class="text-muted">{_ Buffer status is unavailable. _}</p>
                {% endif %}
            </div>
        </div>

        <div class="analytics-health-grid">
            <div><h4>{_ Event processing _}</h4>
                {% if a.status.running %}
                    <p><strong>{{ a.status.accepted }}</strong> {_ recording updates since startup _}<br>
                    <strong>{{ a.status.dropped }}</strong> {_ dropped updates since startup _}</p>
                    <p class="text-muted">{_ One event can update several recordings, so these are not visitor or page-view totals. Counters reset when analytics restarts. _}</p>
                    <p class="{% if a.status.dropped %}text-warning{% else %}text-muted{% endif %}">{_ Dropped updates can result from overload, expired events, configuration changes or failed writes. This is a diagnostic count, not an exact count of lost page views. Incomplete periods are marked in reports. _}</p>
                {% else %}<p class="text-muted">{_ Activity counters are unavailable until analytics resumes. _}</p>{% endif %}
            </div>
            <div><h4>{_ Bounded history _}</h4>
                <p><strong>{{ a.usage.buckets }}</strong> {_ stored time intervals _}</p>
                <p class="text-muted">{_ Each stored time interval holds the counts for all groups in one analytics recording. Storage slots for older intervals are reused, so history stays bounded instead of growing with every visit. Only aggregate counts are stored, not individual events. _}</p>
                <table class="table table-condensed">
                    <caption class="sr-only">{_ History detail and retention _}</caption>
                    <thead><tr><th>{_ Detail _}</th><th>{_ Retention period _}</th></tr></thead>
                    <tbody>
                        <tr><td>{_ 1 minute _}</td><td>{_ 6 hours _}</td></tr>
                        <tr><td>{_ 15 minutes _}</td><td>{_ 7 days _}</td></tr>
                        <tr><td>{_ 1 hour _}</td><td>{_ 30 days _}</td></tr>
                        <tr><td>{_ 1 day _}</td><td>{_ About 5 years _}</td></tr>
                    </tbody>
                </table>
            </div>
        </div>
    </div>
</section>
