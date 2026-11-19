{# Keep labels in one place for the breakdown and both filter selectors. #}
{% for d in dimensions %}
<option value="{{ d|escape }}" data-values="{{ choices[d]|escape }}">{% if d == "country" %}{_ Country — two-letter code, e.g. NL _}
{% elseif d == "device" %}{_ Device — phone, tablet, desktop or unknown _}
{% elseif d == "has_page_id" %}{_ Page ID present — true or false _}
{% elseif d == "http_status" %}{_ HTTP status code — e.g. 200, 404 or 500 _}
{% elseif d == "language" %}{_ Language — language code, e.g. en or nl _}
{% elseif d == "method" %}{_ HTTP method — GET, POST, HEAD and others _}
{% elseif d == "outcome" %}{_ Email outcome — sent, retry, failed or bounced _}
{% elseif d == "page" %}{_ Page — public resource ID _}
{% elseif d == "referrer" %}{_ Referrer — referring hostname, internal or direct/unknown _}
{% elseif d == "status" %}{_ HTTP status class — 2xx success, 4xx client errors, 5xx server errors _}
{% elseif d == "traffic" %}{_ Traffic — crawler, other or unknown _}
{% elseif d == "type" %}{_ Request type — page, api or a custom dispatch type _}
{% else %}{{ d|escape }}{% endif %}</option>
{% endfor %}
