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




//LSOAs
var lsoa = document.getElementById('lambeth-lower-super-output-areas-lsoas'); //change the variable name and elementId
var leaf_widgets_lsoa = Array.prototype.map.call(
    lsoa.querySelectorAll('.leaflet'), //change lnd
  function(ldiv){
    return HTMLWidgets.find('#' + ldiv.id);
  }
);

var map_lsoa = leaf_widgets_lsoa[0].getMap(); //more than one map ie map_i
var legends_lsoa = map_lsoa.controls._controlsById //legends same over all maps

//find all check boxes
var matches_lsoa = lsoa.querySelectorAll('.leaflet-control-layers-selector');


// ADD LISTENERS

//lsoa listener
//add listeners (create for each map)
for (match in matches_lsoa) {
  matches_lsoa[match].onchange = function() {
    var sel = this.labels[0].textContent.trim()
    for (let key in legends_lsoa) {
        map_lsoa.removeControl(legends_lsoa[key]); //map name here
    } //end of for keys
    map_lsoa.addControl(legends_lsoa[sel]);
    //console.log(map_lnd) //for debugging
  } //end of function
} //end of for matches

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