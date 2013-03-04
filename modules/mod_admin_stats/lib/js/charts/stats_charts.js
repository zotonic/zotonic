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
                .text(d.value);
        }};
    }

    function histogram_data(d) {
        return [{ factory: text,
                  datum: {
                      label: "histogram sample count",
                      value: d.count }
                },
                { factory: histogram, datum: d.histogram }];
    }

    function meter_data(d) {
        var now = Date.now();
        var data = this.length > 0 && d3.select(this[0]).datum();
        var values = data || d3.range(299).map(function(i){
            return {x: now - (300 - i)*1000, y: 0} });

        values.push({x: now, y: d.one});

        return [{factory: line, datum: values}];
    }

    function chart_data(d) {
        var type_data = {
            histogram: histogram_data,
            meter: meter_data
        };

        var data = type_data[d.type].apply(this, [d]);

        if (this.length > 0)
            for (var i = 0; i < data.length; i++)
                data[i] = data[i].datum;

        return data;
    }

    function factory(selection) {
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

    factory.update = function(selection) {
        selection.selectAll(".chart-data")
            .data(chart_data)
            .each(function(d) {
                if (this.chart && this.chart.update)
                    this.chart.update.apply(this, [d]);
            });
    }

    return factory;
}
