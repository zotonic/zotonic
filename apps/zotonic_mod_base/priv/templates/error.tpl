{% extends "base.tpl" %}

{% block title %} {{ error_code }} Error {% endblock %}

{% block content %}
{% if error_code == 403 %}
  <h1>{_ No Access _}</h1>
  <p>{_ Sorry, you don’t have access to this page. _}</p>
{% elseif error_code == 410 %}
  <h1>{_ Gone_}</h1>
  <p>{_ Sorry, this page has been deleted. _}</p>
{% elseif error_code == 404 %}
  <h1>{_ That page does not exist _}</h1>
  <p><a href="/">{_ Return to the homepage _}</a></p>
{% else %}
  <h1>{{ error_code }} {_ error _}</h1>

  {% if error_erlang %}
      <p><strong>{{ error_erlang|escape }}</strong></p>
  {% endif %}

  {% if (error_table and m.site.environment /= `production`) or m.acl.is_admin %}
      <h2>{_ Stack trace _}</h2>

      <style type="text/css" nonce="{{ m.req.csp_nonce }}">
          table {
              background-color: transparent;
              border-collapse: collapse;
              border-spacing: 0;
            }

            .tablex {
              width: auto;
              margin-bottom: 18px;
            }
            .tablex th,
            .tablex td {
              padding: 8px;
              line-height: 18px;
              text-align: left;
              vertical-align: top;
              border-top: 1px solid #dddddd;
            }
            .tablex th {
              font-weight: bold;
              color: white;
            }
            .tablex thead th {
              vertical-align: bottom;
              background-color: #0778B0;
            }
            .table-striped tbody tr:nth-child(odd) td,
            .table-striped tbody tr:nth-child(odd) th {
              background-color: #f9f9f9;
            }
            .tablex tbody tr:hover td,
            .tablex tbody tr:hover th {
              background-color: #f5f5f5;
            }
            .template-error td {
                background-color: #fff9f9 !important;
            }
            .template-error td:nth-child(1),
            .template-error td:nth-child(2) {
                font-weight: bold;
            }
            td span,
            .template-error td span {
                font-size: 90%;
                font-weight: normal;
                color: #666;
            }
      </style>

      <table class="tablex table-striped" style="border-collapse: transparent; border-spacing: 0;">
          <thead>
              <tr>
                  <th align="left">{_ Module _}</th>
                  <th align="left">{_ Function/ template _}</th>
                  <th align="left">{_ Arguments _}</th>
                  <th align="left">{_ File _}</th>
              </tr>
          </thead>
          <tbody>
                {% for is_template,mod,func,arg,file in error_table %}
                    {% if is_template %}
                    <tr class="template-error">
                        <td>
                            {{ mod[2]|escape }}
                            {% if mod[1] %}
                              <br><span>{{ mod[1]|escape }}</span>
                            {% endif %}
                        </td>
                        <td>{{ func|escape }}</td>
                        <td>{{ arg|escape }}</td>
                        <td>
                          {% if file[1] %}
                              <a href="file://{{ file[1]|escape }}">{{ file[1]|escape}}</a>{% if file[2] %}<span>:{{ file[2] }}</span>{% endif %}
                          {% endif %}
                        </td>
                    </tr>
                    {% else %}
                    <tr>
                        <td>{{ mod|escape }}</td>
                        <td>{{ func|escape }}</td>
                        <td>{{ arg|escape }}</td>
                        <td>
                          {% if file[1] %}
                              <a href="file://{{ file[1]|escape }}">{{ file[1]|escape}}</a>{% if file[2] %}<span>:{{ file[2] }}</span>{% endif %}
                          {% endif %}
                        </td>
                    </tr>
                    {% endif %}
                {% endfor %}
            </tbody>
      </table>

      <p class="text-muted">
        <span class="glyphicon glyphicon-info-sign"></span>
        {% if m.site.environment == `production` %}
            {_ The stack trace is shown because you are an administrator of this site. _}
        {% else %}
            {_ The stack trace is shown because this is not a production site. _}
        {% endif %}
      </p>

  {% else %}
      {% if error_dump %}
          <pre>{{ error_dump }}</pre>
      {% endif %}
  {% endif %}
{% endif %}
{% endblock %}
