function duraChart(data) {
    var chart = histogram_duration_chart();
    chart.format().x = d3.format(",.1f");
    var ticks = [], i, p, x, r;
    r = (data[0].values[data[0].values.length - 1].x - data[0].values[0].x);
    r /= 10;
    p = -r; log(r);
    for( i = 0; i < data[0].values.length; i++)
    {
        x = data[0].values[i].x;
        if (x - p < r) continue;
        ticks.push(x);
        p = x;
    }
    chart.axis().x.tickValues(ticks);
    return chart;
}

function addGraph(createChart, elem, data) {
    var chart = createChart(data);

    elem.append("h3").text(data[0].key);
    elem.append("div").selectAll("span").data(data[0].info).enter()
        .append("span")
        .text(function(d){
            return d.label + ": " + d.value + (d.unit ? " (" + d.unit + ")" : "") })
        .append("br");

    chart.data(data[0].values)(elem);

    return chart;
}
