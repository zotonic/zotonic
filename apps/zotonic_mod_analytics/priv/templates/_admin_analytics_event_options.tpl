{% for e in events %}
<option value="{{ e.name|escape }}">{% if e.name == "page_request" %}{_ Page requests — successful public HTML pages _}
{% elseif e.name == "api_request" %}{_ API requests — HTTP calls, including errors _}
{% elseif e.name == "request" %}{_ Classified requests — pages, APIs and other dispatch types _}
{% elseif e.name == "smtp" %}{_ SMTP (Email) — sent, retries, failures and bounces _}
{% else %}{{ e.title|escape }}{% endif %}</option>
{% endfor %}
