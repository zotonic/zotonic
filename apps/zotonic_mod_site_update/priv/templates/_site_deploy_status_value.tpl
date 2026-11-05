{% with m.site_update.deployment_status as deployment %}
    {% if deployment %}
        <span class="site-deploy-running text-info{% if not deployment.active or not deployment.deployment %} hidden{% endif %}" role="status" aria-live="polite">
            <span class="fa fa-spinner fa-spin" aria-hidden="true"></span>
            {_ Deployment running on this node _}
        </span>
    {% endif %}
{% endwith %}
