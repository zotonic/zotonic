function metric_meter() {

    function factory_data(d) {
        var now = Date.now();
        var data = line_chart.append_value({
            init: { x: now - 2000, y: 0 },
            x: now,
            y: d,
            max_length: 150,
            series: ["one", "five", "fifteen", "day"],
            datum: this.length > 0 && d3.select(this[this.length - 1]).datum()
        });

        // return our updated values
        return [
            { factory: "text",
              datum: {
                  format: d3.format(".2s"),
                  data: data.series.map(
                      function(serie, i) {
                          return {
                              class: "serie-" + i,
                              label: serie,
                              value: d[serie]
                          };
                      })
                      .concat([
                          { label: "Total count", value: d.count }
                      ])}
            },
            // the index of this line chart is used above
            // when declaring the `var data =`; keep in sync!
            { factory: "line", datum: data.datum }
        ];
    }

    return factory_data;
}
