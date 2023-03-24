// This file is to change the lengends on the leaflet maps in the Cost of Living Data Pack



//London
var lnd = document.getElementById('london-borough'); //change the variable name and elementId
var leaf_widgets_lnd = Array.prototype.map.call(
  lnd.querySelectorAll('.leaflet'), //change lnd
  function(ldiv){
    return HTMLWidgets.find('#' + ldiv.id);
  }
);

var map_lnd = leaf_widgets_lnd[0].getMap(); //more than one map ie map_i
var legends_lnd = map_lnd.controls._controlsById //legends same over all maps

//find all check boxes
var matches_lnd = lnd.querySelectorAll('.leaflet-control-layers-selector');

//Wards
var wrds = document.getElementById('lambeth-wards'); //change the variable name and elementId
var leaf_widgets_wrds = Array.prototype.map.call(
    wrds.querySelectorAll('.leaflet'), //change lnd
  function(ldiv){
    return HTMLWidgets.find('#' + ldiv.id);
  }
);

var map_wrds = leaf_widgets_wrds[0].getMap(); //more than one map ie map_i
var legends_wrds = map_wrds.controls._controlsById //legends same over all maps

//find all check boxes
var matches_wrds = wrds.querySelectorAll('.leaflet-control-layers-selector');

//london listener
//add listeners (create for each map)
for (match in matches_lnd) {
    matches_lnd[match].onchange = function() {
    var sel = this.labels[0].textContent.trim()
    for (let key in legends_lnd) {
        map_lnd.removeControl(legends_lnd[key]); //map name here
    } //end of for keys
    map_lnd.addControl(legends_lnd[sel]);
    //console.log(map_lnd) //for debugging
  } //end of function
} //end of for matches

//wards listener
//add listeners (create for each map)
for (match in matches_wrds) {
    matches_wrds[match].onchange = function() {
      var sel = this.labels[0].textContent.trim()
      for (let key in legends_wrds) {
          map_wrds.removeControl(legends_wrds[key]); //map name here
      } //end of for keys
      map_wrds.addControl(legends_wrds[sel]);
    } //end of function
  } //end of for matches