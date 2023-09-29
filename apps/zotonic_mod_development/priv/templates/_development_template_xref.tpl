
<h3>{_ Syntax errors _}</h3>
<p class="help-block">
    {_ These templates have syntax errors and could not be checked for included files. _}
</p>

<table class="table table-bordered">
    <thead>
        <tr>
            <th>{_ Module _}</th>
            <th>{_ Template _}</th>
            <th>{_ Line _}</th>
            <th>{_ Column _}</th>
            <th>{_ Error _}</th>
        </tr>
    </thead>
    <tbody>
        {% for tpl, error in xref.errors %}
            <tr>
                {% with tpl|split:"/priv/templates/" as ref %}
                    <td>
                        {{ ref[1]|escape_check }}
                    </td>
                    <td>
                        {{ ref[2]|escape_check }}
                    </td>
                {% endwith %}
                <td>
                    {{ error.line }}
                </td>
                <td>
                    {{ error.column }}
                </td>
                <td>
                    {{ error.text|escape_check }}
                </td>
            </tr>
        {% empty %}
            <tr>
                <td colspan="5">
                    <span class="text-muted">
                        {_ All templates are compiling. _}
                    </span>
                </td>
            </tr>
        {% endfor %}
    </tbody>
</table>

<h3>{_ Missing includes _}</h3>
<p class="help-block">
    {% trans "These templates have non optional <tt>{include}</tt> statements that are referring to missing templates."
        include="include"
    %}
</p>

<table class="table table-bordered">
    <thead>
        <tr>
            <th>{_ Module _}</th>
            <th>{_ Template _}</th>
            <th>{_ Line _}</th>
            <th>{_ Included template _}</th>
            <th>{_ Type of include _}</th>
        </tr>
    </thead>
    <tbody>
        {% for tpl, info in xref.missing %}
            <tr>
                {% with tpl|split:"/priv/templates/" as ref %}
                    <td>
                        {{ ref[1]|escape_check }}
                    </td>
                    <td>
                        {{ ref[2]|escape_check }}
                    </td>
                {% endwith %}
                <td>
                    {{ info.line }}
                </td>
                <td>
                    {{ info.template|escape_check }}
                </td>
                <td>
                    {% if info.method == `optional` %}
                        optional
                    {% elseif info.method == `all` %}
                        all
                    {% endif %}
                    {% if info.is_catinclude %}
                        catinclude
                    {% else %}
                        include
                    {% endif %}
                </td>
            </tr>
        {% empty %}
            <tr>
                <td colspan="5">
                    <span class="text-muted">
                        {% trans "All non-optional <tt>{include}</tt> statements are referring to existing templates."
                            include="include"
                        %}
                    </span>
                </td>
            </tr>
        {% endfor %}
    </tbody>
</table>


<h3>{_ Missing optional includes _}</h3>
<p class="help-block">
    {% trans "These templates have <tt>{optional}</tt> statements that are referring to missing templates."
        optional="optional include"
    %}
</p>

<table class="table table-bordered">
    <thead>
        <tr>
            <th>{_ Module _}</th>
            <th>{_ Template _}</th>
            <th>{_ Line _}</th>
            <th>{_ Included template _}</th>
            <th>{_ Type of include _}</th>
        </tr>
    </thead>
    <tbody>
        {% for tpl, info in xref.optional %}
            <tr>
                {% with tpl|split:"/priv/templates/" as ref %}
                    <td>
                        {{ ref[1]|escape_check }}
                    </td>
                    <td>
                        {{ ref[2]|escape_check }}
                    </td>
                {% endwith %}
                <td>
                    {{ info.line }}
                </td>
                <td>
                    {{ info.template|escape_check }}
                </td>
                <td>
                    {% if info.method == `optional` %}
                        optional
                    {% elseif info.method == `all` %}
                        all
                    {% endif %}
                    {% if info.is_catinclude %}
                        catinclude
                    {% else %}
                        include
                    {% endif %}
                </td>
            </tr>
        {% empty %}
            <tr>
                <td colspan="5">
                    <span class="text-muted">
                        {% trans "All <tt>{optional}</tt> statements are referring to existing templates."
                            optional="optional include"
                        %}
                    </span>
                </td>
            </tr>
        {% endfor %}
    </tbody>
</table>

<h3>
    {% trans "Missing {extends} or {overrules}"
         extends="extends"
         overrules="overrules"
    %}
</h3>
<p class="help-block">
    {% trans "These templates have <tt>{extends}</tt> or <tt>{overrules}</tt> statements that are referring to a missing template."
         extends="extends"
         overrules="overrules"
    %}
</p>

<table class="table table-bordered">
    <thead>
        <tr>
            <th>{_ Module _}</th>
            <th>{_ Template _}</th>
            <th>{_ Extends _}</th>
        </tr>
    </thead>
    <tbody>
        {% for tpl, extends in xref.extend_errors %}
            <tr>
                {% with tpl|split:"/priv/templates/" as ref %}
                    <td>
                        {{ ref[1]|escape_check }}
                    </td>
                    <td>
                        {{ ref[2]|escape_check }}
                    </td>
                {% endwith %}
                <td>
                    {{ extends|escape_check }}
                </td>
            </tr>
        {% empty %}
            <tr>
                <td colspan="3">
                    <span class="text-muted">
                        {% trans "All <tt>{extends}</tt> and <tt>{overrules}</tt> statements are referring to existing templates."
                             extends="extends"
                             overrules="overrules"
                        %}
                    </span>
                </td>
            </tr>
        {% endfor %}
    </tbody>
</table>
