function stats_chart_factory() {

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

    function histogram() {
        this.call(
            histogram_duration_chart().call(function() {
                this.format().x = d3.format(",.1f");
                this.ticks().x = update_ticks;
            }));
    }

    function line() {
        this.call(line_chart());
    }

    function text() {
        this.append("span")
            .attr("class", "chart-text")
            .call(function() {
                this.append("span")
                    .attr("class", "chart-label")
                    .text(function(d){ return d.label });
                this.append("span")
                    .attr("class", "chart-value")
                    .text(function(d){ return d.value });
            });
        this.node().chart = { update: function(d) {
            d3.select(this).select(".chart-value")
                //.transition()
                //.attr("style", "opacity: 0")
                //.transition()
                .text(d.value);
                //.attr("style", "opacity: 1")
                //.each("end", function(){
            //d3.select(this).style("opacity", null) });
        }};
    }

    function chart_data(d) {
        if (d.type == "histogram")
            return [{ factory: text, datum: {
                        label: "histogram sample count",
                        value: d.count }
                    },
                    { factory: histogram, datum: d.histogram }];
        return [{factory: line, datum: [{x: Date.now(), y: d.one}]}];
    }

    function chart(selection) {
        selection
            .append("h3").text(function(d){
                return d.system + "." + d.name + " (" + d.type + ")"});

        selection.selectAll("div")
                .data(chart_data)
                .enter()
                .append("div")
                .attr("class", "chart-data")
                .each(function(d){
                    d3.select(this)
                        .datum(d.datum)
                        .call(d.factory);
                });

        return selection;
    }

    chart.update = function(selection) {
        selection.selectAll(".chart-data")
            .data(chart_data)
            .each(function(d) {
                if (this.chart && this.chart.update)
                    this.chart.update.apply(this, [d.datum]);
            });
    }

    return chart;
}
