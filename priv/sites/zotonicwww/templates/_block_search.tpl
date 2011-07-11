<h2>{{ title|default:"Search the documentation" }}</h2>
<form method="get" action="/search">
    <div class="clearfix">
        <input name="q" type="text" style="width: 280px" value="{{ q.q|escape }}" />
    </div>
    <div>
        <button>Search</button>
    </div>
</form>
