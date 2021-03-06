angular.module('app').directive('chordDiagram', ['$window', 'matrixFactory',

function ($window, matrixFactory) {

  var link = function ($scope, $el, $attr) {

    var clickedCountries = [];

    var size = [1000, 1000]; // SVG SIZE WIDTH, HEIGHT
    var marg = [40, 40, 45, 10]; // TOP, RIGHT, BOTTOM, LEFT
    var dims = []; // USABLE DIMENSIONS
    dims[0] = size[0] - marg[1] - marg[3]; // WIDTH
    dims[1] = size[1] - marg[0] - marg[2]; // HEIGHT

    var colors = d3.scale.ordinal()
      .range(['#2C1320', '#432818','#52050a', '#9e0142', '#d53e4f', '#E3879E', '#f46d43', '#fdae61', '#fee08b', '#ffffbf', '#e6f598', '#abdda4', '#66c2a5', '#3288bd', '#5e4fa2', '#03254e', '#4a5759', '#FECEE9', '#5F4B66'])

    var chord = d3.layout.chord()
      .padding(0.02)
      .sortGroups(d3.descending)
      .sortSubgroups(d3.ascending);

    var matrix = matrixFactory.chordMatrix()
      .layout(chord)
      .filter(function (item, r, c) {
        return (item.country1 === r.name && item.country2 === c.name) ||
               (item.country1 === c.name && item.country2 === r.name);
      })
      .reduce(function (items, r, c) {
        var value;
        if (!items[0]) {
          value = 0;
        } else {
          value = items.reduce(function (m, n) {
            return m+n.nbLinkTmp;
          }, 0);
        }
        return {value: value, data: items};
      });

    var innerRadius = (dims[1] / 2) - 100;

    var arc = d3.svg.arc()
      .innerRadius(innerRadius)
      .outerRadius(innerRadius + 20);

    var path = d3.svg.chord()
      .radius(innerRadius);

    var svg = d3.select($el[0]).append("svg")
      .attr("class", "chart")
      .attr({width: size[0] + "px", height: size[1] + "px"})
      .attr("preserveAspectRatio", "xMinYMin")
      .attr("viewBox", "-20 -20 " + size[0] + " " + size[1]);

    var container = svg.append("g")
      .attr("class", "container")
      .attr("transform", "translate(" + ((dims[0] / 2) + marg[3]) + "," + ((dims[1] / 2) + marg[0]) + ")");

    var messages = svg.append("text")
      .attr("class", "messages")
      .attr("transform", "translate(10, 10)")
      .text("Updating...");

    function highlightChords(d) {
      d3.event.preventDefault();
      d3.event.stopPropagation();

      var isEveryBoxChecked = true;
      container.selectAll("path.chord").each(function (p) {
        if (!$scope.filters[p.target._id].hide || !$scope.filters[p.source._id].hide) {
          isEveryBoxChecked = false;
/*          break;*/
        }
      });
     
      if(isEveryBoxChecked) {
        //do some magic
        for (var key in $scope.filters) {
          $scope.filters[key].hide = false;
        }
        $scope.filters[d._id].hide = true;
      }
      else {
        $scope.filters[d._id].hide ? $scope.filters[d._id].hide = false : $scope.filters[d._id].hide = true;
      }

      $scope.updateFilters();
      $scope.$apply();
    }

    $scope.lightChords = function () {

      container.selectAll("path.chord").style("opacity", function (p) {
            return ($scope.filters[p.target._id].hide || $scope.filters[p.source._id].hide) ? 0.9: 0.1;
          });
    }

    function resetChords() {
      d3.event.preventDefault();
      d3.event.stopPropagation();
      container.selectAll("path.chord").style("opacity",0.9);
    }

    function dimChords(d) {
      d3.event.preventDefault();
      d3.event.stopPropagation();
      container.selectAll("path.chord").style("opacity", function (p) {
        if (d.source) { // COMPARE CHORD IDS
          return (p._id === d._id) ? 0.9: 0.1;
        } else { // COMPARE GROUP IDS
          return (p.source._id === d._id || p.target._id === d._id || clickedCountries.indexOf(p.source._id)>-1 || clickedCountries.indexOf(p.target._id)>-1) ? 0.9: 0.1;
        }
      });
    }

    $scope.drawChords = function (data) {

      messages.attr("opacity", 1);
      messages.transition().duration(1000).attr("opacity", 0);

      matrix.data(data)
        .resetKeys()
        .addKeys(['country1', 'country2'])
        .update()

      var groups = container.selectAll("g.group")
        .data(matrix.groups(), function (d) {
                  return d._id;
                }
              );

      var gEnter = groups.enter()
        .append("g")
        .attr("class", "group");

      gEnter.append("path")
        .style("pointer-events", "none")
        .style("fill", function (d) { return colors(d._id); })
        .attr("d", arc);

      gEnter.append("text")
        .attr("dy", ".35em")
        .on("click", highlightChords)
        .text(function (d) {
          return d._id;
        });

      groups.select("path")
        .transition().duration(2000)
        .attrTween("d", matrix.groupTween(arc));

      groups.select("text")
        .transition()
        .duration(2000)
        .attr("transform", function (d) {
          d.angle = (d.startAngle + d.endAngle) / 2;
          var r = "rotate(" + (d.angle * 180 / Math.PI - 90) + ")";
          var t = " translate(" + (innerRadius + 26) + ")";
          return r + t + (d.angle > Math.PI ? " rotate(180)" : " rotate(0)");
        })
        .attr("text-anchor", function (d) {
          return d.angle > Math.PI ? "end" : "begin";
        });

      groups.exit().select("text").attr("fill", "orange");
      groups.exit().select("path").remove();

      groups.exit().transition().duration(1000)
        .style("opacity", 0).remove();

      var chords = container.selectAll("path.chord")
        .data(matrix.chords(), function (d) { return d._id; });

      chords.enter().append("path")
        .attr("class", "chord")
        .style("fill", function (d) {
          return colors(d.source._id);
        })
        .attr("d", path)
        .on("mouseover", chordMouseover)
        .on("mouseout", hideTooltip);

      chords.transition().duration(2000)
        .attrTween("d", matrix.chordTween(path));

      chords.exit().remove()

      function groupClick(d) {
        d3.event.preventDefault();
        d3.event.stopPropagation();
        $scope.addFilter(d._id);
        resetChords();
      }

      function chordMouseover(d) {
        d3.event.preventDefault();
        d3.event.stopPropagation();
        dimChords(d);
        d3.select("#tooltip").style("opacity", 1);
        $scope.updateTooltip(matrix.read(d));
      }

      function hideTooltip() {
        d3.event.preventDefault();
        d3.event.stopPropagation();
        d3.select("#tooltip").style("opacity", 0);
        $scope.updateHighlight();
      }

    }; // END DRAWCHORDS FUNCTION

    function resize() {
      var width = $el.parent()[0].clientWidth;
      svg.attr({
        width: width,
        height: width / (size[0] / size[1])
      });
    }

    resize();

    $window.addEventListener("resize", function () {
      resize();
    });
  }; // END LINK FUNCTION

  return {
    link: link,
    restrict: 'EA'
  };

}]);
