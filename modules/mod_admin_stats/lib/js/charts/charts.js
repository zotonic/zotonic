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


function update_charts(selection, data) {
    function update_ticks(axis, input) {
        var ticks = [], i, p, x, r;
        r = (input[input.length - 1].x - input[0].x) / 10;
        p = -r;
        for( i = 0; i < input.length; i++)
        {
            x = input[i].x;
            if (x - p < r) continue;
            ticks.push(x);
            p = x;
        }

        axis.tickValues(ticks);
    }

    function add_chart(selection) {
        selection
            .attr("class", "chart")
            .append("h3").text(function(d){
                return d.system + "." + d.name + " (" + d.type + ")"});
        selection.each(function (d) {
            if (d.type == "histogram")
                d3.select(this).call(
                    histogram_duration_chart()
                        .data(d.histogram)
                        .call(function(node) {
                            node.chart = this;
                            this.format().x = d3.format(",.1f");
                            this.ticks().x = update_ticks;
                        }, this));
        });
        /*chart.append("div").selectAll("span").data(data[0].info)
            .enter()
            .append("span")
            .text(function(d){
                return d.label + ": " + d.value + (d.unit ? " (" + d.unit + ")" : "") })
            .append("br");*/
    }

    // select charts
    var chart = selection.selectAll("div").data(data);

    // update existing
    chart.each(function(d) {
        if (d.type == "histogram" && this.chart)
            this.chart.update(d.histogram);
    });

    // enter added charts
    chart.enter().append("div").call(add_chart);

    // exit dropped charts
    chart.exit().remove();
}
