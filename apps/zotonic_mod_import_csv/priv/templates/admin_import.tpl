{% extends "admin_base.tpl" %}

{% block title %}{_ Import a CSV or XLSX file _}{% endblock %}

{% block content %}

    <div class="admin-header">
        <h2>{_ Import content _}</h2>
    </div>

    <div class="widget">
        <div class="widget-content">
            {% all include "_admin_import_button.tpl" %}

            <hr>

            <h3>{_ About the columns of the spreadsheet data _}</h3>

            <p>
                {_ Every row in the CSV or XLSX file contains a single page. The columns are the properties. _}<br>
                {_ The following properties must be present: _}
            </p>

            <table class="table table-compact table-striped" style="width: auto">
                <tr>
                    <th>{_ Property _}</th>
                    <th>{_ Description _}</th>
                </tr>
                <tr>
                    <td><code>name</code></td>
                    <td>{_ The uniqie name of the page, used to identify the page. _}</td>
                </tr>
                <tr>
                    <td><code>category</code></td>
                    <td>{_ The category of the page. Examples: <code>article</code> or <code>text</code>. _}</td>
                </tr>
            </table>

            <p>{_ Well known properties are: _}</p>
            <table class="table table-compact table-striped" style="width: auto">
                <tr>
                    <th>{_ Property _}</th>
                    <th>{_ Description _}</th>
                </tr>
                <tr>
                    <td><code>title</code></td>
                    <td>{_ The title of the page. _}</td>
                </tr>
                <tr>
                    <td><code>summary</code></td>
                    <td>{_ The summary of the page. _}</td>
                </tr>
                <tr>
                    <td><code>body</code></td>
                    <td>{_ The HTML main text of the page. _}</td>
                </tr>
                <tr>
                    <td><code>name_first</code></td>
                    <td>{_ If the page is a person, the first name. _}</td>
                </tr>
                <tr>
                    <td><code>name_surname</code></td>
                    <td>{_ If the page is a person, the last or family name. _}</td>
                </tr>
                <tr>
                    <td><code>email</code></td>
                    <td>{_ The email address associated with the page. _}</td>
                </tr>
                <tr>
                    <td><code>page_path</code></td>
                    <td>{_ The custom path for the URL of the page, for example <code>/my/page</code> _}</td>
                </tr>
            </table>

            <p>
                {_ Multilingual properties can be set by specifying the language after the property name. _}<br>
                {_ For example, the English title would have the column name: <code>title$en</code> _}
            </p>

        </div>
    </div>

{% endblock %}
