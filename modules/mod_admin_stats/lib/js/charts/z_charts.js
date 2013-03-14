var z_charts = {};

(function($){

    // define property setter/getter function
    $.define_property = function(obj, name, ret) {
        return function() {
            if (!arguments.length)
                return obj[name];
            obj[name] = arguments[0];
            return ret;
        };
    };

    // assign properties to chart
    $.proxy_properties = function (src, dst) {
        var name;
        for (name in src) {
            if (src.hasOwnProperty(name))
                dst[name] = this.define_property(src, name, dst);
        }
    };

    // update all charts in the selection with new data
    $.update = function(selection, data, chart_factory) {
        // select charts and assign data
        var chart = selection.selectAll("div.chart").data(data);

        // update existing
        chart.call(chart_factory.update);

        // enter added charts
        chart.enter().append("div")
            .attr("class", "chart")
            .call(chart_factory);

        // exit dropped charts
        chart.exit().transition()
            .attr("style", "opacity=0")
            .remove();
    };

})(z_charts);
