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

    function test() {
        this.append("h4").text(function(d){ return "initial count: " + d });
        this.node().chart = { update: function(d) {
            d3.select(this).select("h4")
                .text("updated count: " + d);
        }};
    }

    function chart(selection) {
        selection
            .append("h3").text(function(d){
                return d.system + "." + d.name + " (" + d.type + ")"});

        selection.each(function (d) {
            if (d.type == "histogram") {
                d3.select(this).selectAll("div")
                    .data([
                        { factory: histogram, datum: d.histogram },
                        { factory: test, datum: d.count }
                    ])
                    .enter()
                    .append("div")
                    .attr("class", "chart-data")
                    .each(function(d){
                        d3.select(this)
                            .datum(d.datum)
                            .call(d.factory);
                    });
            }
        });
        /*chart.append("div").selectAll("span").data(data[0].info)
          .enter()
          .append("span")
          .text(function(d){
          return d.label + ": " + d.value + (d.unit ? " (" + d.unit + ")" : "") })
          .append("br");*/

        return selection;
    }

    chart.update = function(selection) {
        selection.selectAll(".chart-data")
            .data(function(d){
                if (d.type == "histogram")
                    return [d.histogram, d.count];
                return [];
            })
            .each(function(d){
                if (this.chart && this.chart.update)
                    this.chart.update.apply(this, [d]);
            });
    }

    return chart;
}
