(function($) {

    function metric_data(d) {
        var metric = metric_data.types[d.type];
        if (metric)
            return metric.apply(this, [d]);
        return [];
    }

    metric_data.types = {
        histogram: metric_histogram(),
        meter: metric_meter()
    };

    function update(d) {
        var metrics = d3.select(this)
            .selectAll("div.metric")
            .data(metric_data);

        // update existing
        z_charts.update(metrics);

        // add new
        metrics.enter()
            .append("div")
            .attr("class", "metric")
            .call(z_charts.factory)

        // remove dropped
        metrics.exit()
            .remove();
    }

    // The factory function creating all charts and stuff
    function factory(selection) {
        selection
            .call(z_charts.create_chart, {update: update})
            .append("h3")
            .text(function(d){
                return d.system + " " + d.name });

        return selection.each(update);
     }

    // store our factory in the provided name space
    $.stats = factory;

})(z_charts.factories);
