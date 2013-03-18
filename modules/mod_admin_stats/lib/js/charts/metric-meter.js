function metric_meter() {

    function factory_data(d) {
        var series = ["one", "five", "fifteen", "day"];
        var now = Date.now();

        // for the line chart, we append each new value to a list
        var data = this.length > 0 && d3.select(this[this.length - 1]).datum();

        // if we don't have a list yet, initialize a new one
        if (!data)
            data = series.map(function(){
                return [{x: now - 1000, y: 0}] });

        // for each line series, append the new value
        data.forEach(
            function(s, i) {
                s.push({x: now, y: d[this[i]]})
            },
            series);

        // return our updated values
        return [
            { factory: "wrap",
              datum: {
                  class: "pull-left",
                  data: series.map(
                      function(serie, i) {
                          return {
                              factory: "text",
                              datum: {
                                  class: "serie-" + i,
                                  label: serie,
                                  value: d[serie] }
                          };
                      })
                      .concat([
                          { factory: "text",
                            datum: { label: "Total count", value: d.count }
                          }
                      ])}
            },
            // the index of this line chart is used above
            // when declaring the `var data =`; keep in sync!
            { factory: "line", datum: data }
        ];
    }

    return factory_data;
}
