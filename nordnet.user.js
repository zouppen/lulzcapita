// ==UserScript==
// @name          LulzCapita for Nordnet
// @namespace     http://zouppen.iki.fi/2011/nordnet
// @description   A Greasemonkey script that synchronizes your portfolio data on Nordnet to LulzCapita 
// @include       https://www.nordnet.fi/mux/web/user/overview.html
// ==/UserScript==

var portfolioId = document.evaluate("//div/@portfolio", document, null, XPathResult.ANY_TYPE, null).iterateNext().textContent;

var muut = document.evaluate("//div[div/@class='subBar']", document, null, XPathResult.ANY_TYPE, null).iterateNext();

// Sends provided portfolio string to LulzCapita
function sendToLulz(portfolio) {
    "use strict";
    
    GM_xmlhttpRequest({
	method: "POST",
	url: "http://capita.lulz.fi/portfolio_sink",
	data: portfolio,
	headers: {
	    "Content-Type": "text/plain; charset=UTF-8",
	    "PortfolioFormat": "nordnet",
	    "PortfolioID": portfolioId
	},
	onload: function(result) {
	    console.log("Synchronized to LulzCapita");
	    alert("Salkku synkronoitu LulzCapitaan onnistuneesti!");
	},
	onerror: function(result) {
	    console.log("Failed to sync to LulzCapita");
	    alert("Tapahtumien lähettäminen tietokantaan epäonnistui: "+
		  result.statusText);
	}
    });
    //alert("Salkku lähetetään palvelimelle");
}

// Fetches data from Nordnet and sends it to the provided callback.
function readFromNordnet(callback) {
    "use strict";
    GM_xmlhttpRequest({
	method: "GET",
	url: "/mux/laddaner/transaktionsfil.html?year=all&month=all&trtyp=all&vp=all&curr=all&sorteringsordning=fallande&sortera=datum&startperiod=01-01-2000&endperiod=01-01-2020",
	overrideMimeType: "text/plain; charset=ISO-8859-1",
	onload: function(result) {
	    console.log("Read portfolio successfully from Nordnet");
	    callback(result.responseText);
	},
	onerror: function(result) {
	    console.log("Failed to read portfolio from Nordnet");
	    alert("Tapahtumien lukeminen Nordnetistä epäonnistui: " +
		  result.statusText);
	}
    });
}

function lulzSync () {
    "use strict";
    readFromNordnet(sendToLulz);
}

// Adding a link and a listener on main page (in Finnish).
var div = document.createElement("div");
div.setAttribute("class", "section");
var href = document.createElement("a");
href.setAttribute("style", "cursor:pointer;");
href.textContent = "Synkronoi salkkusi LulzCapitaan";
href.addEventListener("click", lulzSync, false);
div.appendChild(href);
muut.appendChild(div);
