{% extends "base.tpl" %}

{% block title %} {{ error_code }} Error {% endblock %}

{% block content %}
{% if error_code == 403 %}
	<h1>{_ No Access _}</h1>

	<p>{_ Sorry, you don’t have access to this page. _}</p>
{% else %}
	<h1>{{ error_code }} {_ error _}</h1>

	{% if error_erlang %}
	    <p>{_ Reason _}: <strong>{{ error_erlang|escape }}</strong></p>
	{% endif %}
	
	{% if error_table %}
	    <h2>{_ Stack trace _}</h2>
	    
	    <style type="text/css">
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
                        <td>{{ mod|escape }}</td>
                        <td>{{ func|escape }}</td>
                        <td>{{ arg|escape }}</td>
                        <td>{{ file|escape}}</td>
                    </tr>
                    {% else %}
                    <tr>
                        <td>{{ mod|escape }}</td>
                        <td>{{ func|escape }}</td>
                        <td>{{ arg|escape }}</td>
                        <td>{{ file|escape}}</td>
                    </tr>
                    {% endif %}
                {% endfor %}
            </tbody>
	    </table>
    {% else %}
	    <pre>{{ error_dump }}</pre>
	{% endif %}
{% endif %}
{% endblock %}
