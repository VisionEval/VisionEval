
// The VEModel use case is to call the visualize function which will start the browser with a
// websocket connection, load the visualizer page, use sendCommand to create the four required
// Javascript objects, then use sendCommand to execute the VisualVE command below.

// Probably when we run the query, we should construct a single .JS file that defines the four data
// objects (so we can re-visualize without re-running the query). So creating that file can be a
// variant of "export" (another target of which might be Tableau input data).

// We can have a different VisualVE function for Category-based scenarios (the complex case
// initially implemented) and for manual or folder-based scenarios, where we have one column
// per scenario and the Y-axis has the metric value ticks - we just see each scenario lined
// up based on its metric.  We get one input "ScenarioGroup" which has one bar per scenario
// and we can click it in or out. The bottom table shows Scenario Name, no level categories,
// and the numeric values for each metric.

// The following will auto-build the visualizer based on attempts to load the variables from
// specific external files (see visualizer.html).

$(document).ready( function() {
  if (
      typeof catconfig !== 'undefined' &&
      typeof scenconfig !== 'undefined' &&
      typeof outputconfig !== 'undefined' &&
      typeof VEdata !== 'undefined' ) {
    VisualVE();
  }
});

// Define the VisualVE (Visualizer) function that operates on loaded data
// Expect catconfig, scenconfig, outputconfig and VEdata defined globally

VisualVE = function( ) {
  var scenarioData = scenconfig;
  var categoryData = catconfig;       // from category-cfg.js = Scenario Group elements ("Community Design" etc.) - configures Scenario Input Levels
  var outputData = outputconfig;   // from output-cfg.js = Output Measure elements ( "DVMT Per Capita", etc.) - configures Model Outputs
  // VEdata from verspm.js

  var categoryInputChartCount = categoryData.length;
  var outputMeasureChartCount = outputData.length;

  $('#ScenarioDisplayTitle').html(scentitle)

  // Check if category Levels are 0-based or 1-based
  // TODO: Ultimately rewrite description appending not to rely on numeric levels
  // Later use of levels only checks them for "sameness", and they could be text
  var levelOffset = 0; // Integer value of first actual scenarioData level (for compatibility with old samples)
  if ( parseInt(categoryData[0].LEVELS[0].NAME[0])>0 ) levelOffset = 1;

  var scenData = [];
  var DataObjects = [];
  $.each(categoryData,function(index,values) {
      var scenarioName = values.NAME;         
      var scenarioDescription = values.DESCRIPTION;           
      var scenarioLevels = values.LEVELS;
      scenData[index] = scenarioName; 
      $('#ScenarioInputCharts')
      .append(
              "<div class='col-sm-2 pietitle policy' id='Scenario"+index+"'>"+
              "<div>"+
              "<h5 class='text-center'><span class='pietitle' id='scen"+index+"-popover'></span></h5>"+
              "</div>"+            
              "<div id='scen"+index+"-popover-content' style='display: none'>"+
              "</div>"+
              "</div>"
             );
      DataObjects[index] = {
          "NAME" : scenarioName,
          "DESC": scenarioDescription,
          "LEVELS": values.LEVELS                      
      };

      $('#scen' + index + '-popover').append(scenarioName+"<span class=\"glyphicon glyphicon-info-sign\"></span>");            
      $('#scen' + index + '-popover-content').append('<p><strong>' + scenarioName + ':</strong> ' + scenarioDescription + '</p>');
      $.each(scenarioLevels, function(i,val){
          $.each(val.INPUTS,function(k,v){
              $.each(scenarioData, function(idx,scenario){
                  if(v.NAME[0] == scenario.NAME[0]) {
                      $('#scen' + index + '-popover-content')
                      .append('<p><strong>L'+i+': ' + scenario.LABEL + ':</strong> ' + scenario.LEVELS[parseInt(v.LEVEL)-levelOffset].DESCRIPTION + '</p>');
                      // hange to 0-based indexing
                      // [NOTE weird dependency on text names really being index-like integers
                      // (i.e. 0 to N items).
                      // The following line must be in place instead to run the "hidden" visioneval.js
                      //.append('<p><strong>L'+i+': ' + scenario.LABEL + ':</strong> ' + scenario.LEVELS[parseInt(v.LEVEL)-1].DESCRIPTION + '</p>');
                  }
              });
          });
      });
      $("#scen"+index+"-popover").popover({
          trigger: "hover",
          html: true,
          placement: "bottom",
          content: function() {
              return $('#scen'+index+'-popover-content').html();
          }
      });
  });

  // Add low-level scenarios to DataTable
  var scenarioTableColumns = [];
  $.each(scenarioData,function(index,values) {
      scenarioTableColumns[index] = function(d) { return d[values.NAME]; }
      var scenarioName = values.NAME+":"+values.LABEL;
      $('#Scenario-Results thead tr.header').append("<th>"+scenarioName+"</th>");
  });

  var outputCol = [];
  var outputXticks = [];
  var dataTableColumns = [];
  $.each(outputData, function(index,values){
      outputCol[index] = values.NAME;
      dataTableColumns[index] = function(d) { return d[outputCol[index]]; };
      outputXticks[index] = values.XTICKS;
      var outputName = values.DISPLAYNAME;
      var outputLabel = values.LABEL;
      var outputDesc = values.DESCRIPTION;
      var outputInst = values.INSTRUCTIONS;
      var outputMetric = values.METRIC;
      var outputUnit = values.UNIT;
      $('#OutputInputCharts').append("<div class='col-sm-3' id='Output"+index+"'>"+
                                     "<div>"+
                                     "<h5 class='text-center'><span class='bartitle' id='output"+index+"-popover'>"+"</span></h5>"+
                                     "<h5 class='text-center sm-margin'><span id='output"+index+"_subtitle' class='subtitle'><span></h5>"+                
                                     "</div>"+
                                     "<div id='output"+index+"-popover-content' style='display: none'>"+
                                     "</div>"+
                                     "</div>");
      $('#output'+index+'-popover').append(outputName+ "<span class=\"glyphicon glyphicon-info-sign\"></span>");
      $('#output'+index+'_subtitle').append(outputMetric+' = <span id="output'+index+'_stat" class="stat">'+'</span> '+outputUnit);  
      $('#output'+index+'-popover-content').append('<p><strong>'+outputLabel+ ": </strong>"+outputDesc+'</p>');
      $('#outputinstlist').append('<li><strong>'+outputName+'</strong>: '+outputInst+'</li>');
      $('#Scenario-Results thead tr.header').append("<th>"+values.NAME+"</th>");
      $("#output"+index+"-popover").popover({
          trigger: "hover",
          html: true,
          placement: "bottom",
          content: function() {
              return $('#output'+index+'-popover-content').html();
          }
      });
  });
  dataTableColumns = scenarioTableColumns.concat(dataTableColumns);

  // Establish the crossfilter
  //--------------------------
  var facts = crossfilter(VEdata);

  // Set up all of the dc.js chart objects
  //--------------------------------------
  // Set up the pie chart objects to display scenario input levels
  var scenarioBarChart = new Array(categoryInputChartCount);
  $.each(scenarioBarChart,function(index,value) {
      scenarioBarChart[index] = dc.barChart("#Scenario"+index);
  });

  // Set up the bar chart objects to display scenario output levels
  var outputBarChart = new Array(outputMeasureChartCount);
  $.each(outputBarChart, function(index,value) {
      outputBarChart[index] = dc.barChart("#Output"+index);
  });

  // Set up the number objects to display output averages
  var outputAveNum = new Array(outputMeasureChartCount);
  $.each(outputAveNum, function(index,value) {
      outputAveNum[index] = dc.numberDisplay("#output"+index+"_stat");
  });

  // Set up table to display values for selected scenarios
  var scenarioTable = dc.dataTable("#Scenario-Results");

  // Set up display of total selected scenarios
  var all = facts.groupAll();
  dc.dataCount(".num-selected-scenarios")
  .dimension(facts)
  .group(all);

  // Establish crossfilter dimensions for the scenario data
  //---------------------------------------------------------

  // Establish the scenario input dimensions to be displayed and filtered
  var scenarioDim = Array(categoryInputChartCount);
  $.each(scenarioDim,function(index,value) {
      scenarioDim[index] = facts.dimension( function(d) {
          var myData = DataObjects[index];
          var thislevel;
          var thisismylevel = false;
          var scnCnt = 0;
          $.each(myData.LEVELS, function(key,value) {
              var inputs = value.INPUTS;
              scnCnt =0;                   
              $.each(inputs,function(idx,i){
                  if(d[i.NAME] == i.LEVEL){
                      scnCnt++;                                                       
                  }
              });
              if(scnCnt == inputs.length){                        
                  thislevel = value.NAME;
                  thisismylevel = true;                       
              }
          });       
          if(thisismylevel ==true){
              return  thislevel;
          }      
      } )
  });

  // Establish the scenario output dimensions to be displayed and filtered
  // getOutputCol = function(outputIndex) { return function(d) { return d[outputCol[outputIndex]];} }
  // Again restructure so facts.dimension( getOutputCol(outputIndex) )
  var outputDim = Array(outputMeasureChartCount);
  $.each(outputDim, function(index,value) {
      outputDim[index] = facts.dimension( function(d) {return d[outputCol[index]];} );
  });

  // Establish the group statistics to display
  //------------------------------------------
  // Establish the scenario input group statistics (counts) to be displayed as pie wedges
  // Index into scenarioDim based on outputIndex (or just do for each element in the array)
  var scenarioGroup = Array(categoryInputChartCount);
  $.each(scenarioGroup,function(index,value) { scenarioGroup[index] = scenarioDim[index].group(); });

  var scenarioGroupWhole = Array(categoryInputChartCount);
  $.each(scenarioGroupWhole,function(index,value) { scenarioGroupWhole[index] = scenarioGroup[index].top(Infinity); });

  var scenarioHash = Array(categoryInputChartCount);
  $.each(scenarioHash,function(index,value) { scenarioHash[index] = {}; });

  $.each(scenarioGroupWhole,function(index,value) {
      scenarioGroupWhole[index].forEach( function(p,i) {
          scenarioHash[index][p.key] = p.value;
      } );
  });

  // Establish the bin dimensions for bar charts (to be used in grouping)
  var measures = outputCol;
  var bins = {};
  var calcBinning = function(data, measure, nBins) {
      var xExtent = d3.extent(data, function(d) {return d[measure];});
      var xWidth = (xExtent[1] - xExtent[0]) / nBins;
      return { xExtent: xExtent, xWidth: xWidth };
  };
  for( var i=0; i<measures.length; i++ ) {
      bins[measures[i]] = calcBinning( VEdata, measures[i], 15 );
  };

  // Establish the output group statistics (counts) to be displayed in bar charts
  var outputReduceGroup = Array(outputMeasureChartCount);
  $.each(outputReduceGroup, function(index,value) {
      outputReduceGroup[index] = outputDim[index].group( function(d) {
          return Math.floor(d / bins[outputCol[index]].xWidth) * bins[outputCol[index]].xWidth;
      });
  });

  // Define reduce functions to calculate output measure averages for filtered selections
  function addAvg(attr) {
      return function(p,v) {
          ++p.count;
          p.sum += v[attr];
          p.avg = p.sum/p.count;
          return p;
      };
  }
  function remAvg(attr) {
      return function(p,v) {
          --p.count;
          p.sum -= v[attr];
          p.avg = p.sum/p.count;
          return p;
      };
  }
  function iniAvg() {
      return {count:0, sum:0, avg:0};
  }

  // Establish the output group output measure averages
  var allDim = facts.dimension( function(d) {return d;} );
  //allDim = facts.dimension( function(d) {return Math.round(d.GHGReduction / d.GHGReduction);} ),
  var outputReduceAveGroup = Array(outputMeasureChartCount);
  $.each(outputReduceAveGroup, function(index,value) {
      outputReduceAveGroup[index] = allDim.group().reduce(addAvg(outputCol[index]), remAvg(outputCol[index]), iniAvg);
  });

  // Plot the input pie charts
  //--------------------------
  // Define layout variables that are common to all the pie charts
  var
     pieWidth = 160,
     pieHeight = 160,
     pieRadius = 60,
     pieInnerRadius = 20,
     pieMargins = {top: 10, right: 10, bottom: 10, left: 10}
  ;       
  var   upperBarMargins = {top: 0, right: 10, bottom: 20, left: 15}

  // Define the input "pie" charts (using bars in this case)
  $.each(scenarioBarChart,function(index,value) {
      scenarioBarChart[index].width(pieWidth)
      .height(200)      
      .stack(18)
      .x(d3.scaleBand())
      .xUnits(dc.units.ordinal)
      .dimension(scenarioDim[index])
      .group(scenarioGroup[index])	        
      .gap(10)
      .yAxisPadding('10%')
      .renderLabel(true)          
      .label(function (d){return "L" + d.data.key;})
      .stack(scenarioGroup[index], "Total Items",function(d) { 
          //only add the stacked data to filtered bars that have data.
          var id = d.key;
          return scenarioHash[index][id] - d.value; //only add the difference between the filter and totals
      });
  });

  // Plot the output bar charts
  //---------------------------
  // Define layout variables that are common to all the bar charts
  var
     barWidth = 250,
     barHeight = 120,
     barMargins = {top: 10, right: 10, bottom: 20, left: 15},
     barGap = 35,
     barX = 5
            ;

  // Define the output bar charts
  $.each(outputBarChart,function(index,value) {
      outputBarChart[index].width(barWidth)
      .height(barHeight)
      .margins(barMargins)
      .dimension(outputDim[index])
      .group(outputReduceGroup[index])
      .transitionDuration(200)
      .centerBar(true)
      .gap(barGap)
      .x(d3.scaleLinear().domain(bins[outputCol[index]].xExtent))
      .elasticY(true)
      .xAxis().tickFormat();
      outputBarChart[index].xAxis().ticks(outputXticks[index]);
      outputBarChart[index].yAxis().ticks(5);
      outputBarChart[index].xUnits(function(){return barX;});
      if ( (index % 4)==0 ) outputBarChart[index].yAxisLabel("# Scenarios")
  });


  // Display the average values for outputs
  //------------------------------------------------------------
  $.each(outputAveNum,function(index,value) {
      outputAveNum[index].group(outputReduceAveGroup[index]).valueAccessor(function(p) {return p.value.avg;});
  });

  // Display the scenario data table
  //---------------------------------------------------------

  var sortColumn = function(d) {return d[outputCol[0]];}
  scenarioTable.width(960).height(800)
  .dimension(outputDim[0])
  .group(function(d) {return "";})
  .columns(dataTableColumns)            
  .sortBy(sortColumn)
  .order(d3.ascending)
  .size(500);

  // Render all of the graphic elements
  //-----------------------------------
  dc.renderAll();
}
