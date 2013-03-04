function stats_chart_factory() {

    // Chart factory helpers

    // creates a duration histogram chart
    function histogram() {
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

        this.call(
            histogram_duration_chart().call(function() {
                this.format().x = d3.format(",.1f");
                this.ticks().x = update_ticks;
            }));
    }

    // creates a line chart
    function line() {
        this.call(line_chart());
    }

    // creates a dynamic "label value" text element
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

    // Chart data helpers

    // create data for the histogram chart
    function histogram_data(d) {
        return [
            { factory: text, datum:
              { label: "Min", value: d.min }
            },
            { factory: text, datum:
              { label: "Max", value: d.max }
            },
            { factory: text, datum:
              { label: "Mean (geometric)", value: d.mean.geometric }
            },
            { factory: text, datum:
              { label: "Sample count", value: d.count }
            },
            { factory: histogram, datum: d.histogram }
        ];
    }

    // create data for the line chart
    function meter_data(d) {
        var now = Date.now();
        var data = this.length > 0 && d3.select(this[1]).datum();
        var values = data || d3.range(299).map(function(i){
            return {x: now - (300 - i)*1000, y: 0} });

        values.push({x: now, y: d.one});

        return [
            { factory: text, datum:
             { label: "Total count", value: d.count }
            },
            { factory: line, datum: values }
        ];
    }

    // chart data middle man
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

    // The factory function creating all charts and stuff
    function factory(selection) {
        selection
            .append("h3").text(function(d){
                return d.system + " " + d.name });

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

    // The update function propagating new data to all charts and stuff
    factory.update = function(selection) {
        selection.selectAll(".chart-data")
            .data(chart_data)
            .each(function(d) {
                if (this.chart && this.chart.update)
                    this.chart.update.apply(this, [d]);
            });
    }

    // return the completed factory
    return factory;
}
