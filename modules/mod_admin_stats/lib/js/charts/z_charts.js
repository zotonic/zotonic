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
                dst[name] = $.define_property(src, name, dst);
        }
    };

    // apply updates
    $.update = function(selection) {
        selection.each(function(d) {
            if (d.datum)
                d3.select(this).datum(d.datum);

            if (this.chart && this.chart.update)
                this.chart.update.apply(this, [d.datum || d]);
        });
    };

    // update all charts in the selection with new data
    $.process = function(selection, data, factory) {
        // select charts and assign data
        var charts = selection.selectAll("div.z-chart").data(data);

        // update existing
        charts.call($.update);

        // enter added charts
        charts.enter().append("div")
            .attr("class", "z-chart")
            .call(factory);

        // exit dropped charts
        charts.exit().transition()
            .attr("style", "opacity=0")
            .remove();
    };

    // chart factory helper
    $.factory = function(selection) {
        function lookup_factory(f) {
            if (typeof f === 'function')
                return f;

            if (typeof f === 'string')
                f = $.factories[f];

            if (typeof f !== 'function')
                throw "bad factory function";

            return f;
       };

        return selection.each(
            function(d) {
                d3.select(this)
                    .datum(d.datum)
                    .call(lookup_factory(d.factory));
            });
    };

    // store chart object in node
    $.create_chart = function(selection, obj) {
        return selection.each(function() {
            this.chart = obj;
        });
    };

    // name space for chart factories
    $.factories = {};

    // creates a line chart
   $.factories.line = function(selection) {
       return line_chart().call(
           function() {
               this
                   .width(300)
                   .height(100);
               this.axis().x.ticks(5);
               this.axis().y.ticks(3);
           })(selection);
    };

    // Creates a dynamic "label value" text element
    $.factories.text = function(selection) {
        return selection
            .call($.create_chart, { update: text_update })
            .each(text_update);
    };

    function text_update(d) {
        function format_value(v) {
            return d.format ? d.format(v.value) : v.value;
        }

        var texts = d3.select(this).selectAll("span.chart-text")
            .data(d.data || [d]);

        // update
        texts.select(".chart-value").text(format_value);

        // add
        texts.enter()
            .append("span")
            .attr("class", function(d) {
                return "chart-text " + (d.class||"") })
            .call(function() {
                this.append("span")
                    .attr("class", "chart-label")
                    .text(function(v) { return v.label });
                this.append("span")
                    .attr("class", "chart-value")
                    .text(format_value);
            });

        // delete
        texts.exit()
            .remove();
    }

    // Group a set of charts in within a element
    $.factories.wrap = function(selection) {
        return selection
            .call($.create_chart, { update: wrap_update })
            .each(function(d) {
                if (d.class)
                    d3.select(this).classed(d.class, true);
                wrap_update.apply(this, [d]);
            });
    };

    function wrap_update(d) {
        var wraps = d3.select(this).selectAll("div.z-wrap")
            .data(d.data);
        wraps
            .call($.update)
            .enter()
            .append("div").attr("class", "z-wrap")
            .call($.factory);
        wraps.exit()
            .remove();
    }

})(z_charts);
