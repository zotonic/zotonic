<input type="hidden" class="col-lg-6 col-md-6" name="{{ blk.name }}" id="{{ #id }}" value="{{ answers[blk.name]|escape }}" />
<div class="row short-answer-date">
    {% if blk.is_required %}
        {% validate id=#id name=blk.name type={presence} %}
    {% endif %}

    <div class="col-sm-2">
        <select id="{{ #year }}" class="form-control">
            <option></option>
            {% with now|date:"Y" as year %}
            {% for i in year|range:(year-110):(0-1) %}
                <option>{{ i }}</option>
            {% endfor %}
            {% endwith %}
        </select>
    </div>
    <div class="col-sm-6">
        <select id="{{ #month }}" class="form-control">
            <option></option>
            <option value="1">{_ January _}</option>
            <option value="2">{_ February _}</option>
            <option value="3">{_ March _}</option>
            <option value="4">{_ April _}</option>
            <option value="5">{_ May _}</option>
            <option value="6">{_ June _}</option>
            <option value="7">{_ July _}</option>
            <option value="8">{_ August _}</option>
            <option value="9">{_ September _}</option>
            <option value="10">{_ October _}</option>
            <option value="11">{_ November _}</option>
            <option value="12">{_ December _}</option>
        </select>
    </div>
    <div class="col-sm-2">
        <select id="{{ #day }}" class="form-control">
            <option></option>
            {% for i in 1|range:31 %}
                <option>{{ i }}</option>
            {% endfor %}
        </select>
    </div>
</div>

{% javascript %}
    var d = $('#{{ #id }}').val();
    if (d != '') {
        d = new Date(d);
        $('#{{ #year }}').val(d.getFullYear());
        $('#{{ #month }}').val(d.getMonth()+1);
        $('#{{ #day }}').val(d.getDate());
    }
    $('#{{ #year }},#{{ #month }},#{{ #day }}').change(function() {
        var y = $('#{{ #year }}').val();
        var m = $('#{{ #month }}').val();
        var d = $('#{{ #day }}').val();

        if (y && m && d) {
            d = new Date(y, m-1, d);
            $('#{{ #year }}').val(d.getFullYear());
            $('#{{ #month }}').val(d.getMonth()+1);
            $('#{{ #day }}').val(d.getDate());
            $('#{{ #id }}').val(
                '' + d.getFullYear()
                + '-' + ('0'+(d.getMonth()+1)).slice(-2)
                + '-' + ('0'+d.getDate()).slice(-2)
            );
        } else {
            $('#{{ #id }}').val('');
        }
    });
{% endjavascript %}
