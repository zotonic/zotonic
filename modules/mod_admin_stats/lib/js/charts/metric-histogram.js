function metric_histogram() {
    // creates a duration histogram chart
    function histogram(selection) {
        function update_ticks(axis, input) {
            var ticks = [], i, p, x, r;
            r = (input[input.length - 1].x - input[0].x) / 5;
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

        selection.classed("pull-left", true);
        return histogram_duration_chart().call(
            function() {
                this.format().x = d3.format(",.1f");
                this.ticks().x = update_ticks;
                this
                    .width(300)
                    .height(100)
                    .axis().y.ticks(3);
            })(selection);
    }

    function factory_data(d) {
        var now = Date.now();
        var data = line_chart.append_value({
            init: { x: now - 2000, y: 0 },
            x: now,
            y: d.mean,
            max_length: 150,
            series: ["geometric"],
            datum: this.length > 0 && d3.select(this[this.length - 1]).datum()
        });

        return [
            { factory: "text",
              datum: {
                  format: d3.format(".2s"),
                  data: [
                      { label: "Min", value: d.min },
                      { label: "Max", value: d.max },
                      { label: "Mean (geometric)",
                        value: d.mean.geometric,
                        class: "serie-0" },
                      { label: "Sample count", value: d.count }
                  ]}
            },
            { factory: histogram, datum: d.histogram },
            { factory: "line", datum: data.datum }
        ];
    }

    return factory_data;
}
