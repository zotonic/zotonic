function metric_histogram() {
    // creates a duration histogram chart
    function histogram(selection) {
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

    function factory_data(d) {
        return [
            { factory: "wrap",
              datum: {
                  class: "pull-left",
                  data: [
                      { factory: "text",
                        datum: { data: [
                            { label: "Min", value: d.min },
                            { label: "Max", value: d.max }]}
                      },
                      { factory: "text",
                        datum: {
                            label: "Mean (geometric)",
                            value: d.mean.geometric }
                      },
                      { factory: "text",
                        datum: {
                            label: "Sample count",
                            value: d.count }
                      }]}
            },
            { factory: histogram, datum: d.histogram }
        ];
    }

    return factory_data;
}
