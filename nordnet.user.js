// ==UserScript==
// @name          LulzCapita for Nordnet
// @namespace     http://zouppen.iki.fi/2011/nordnet
// @description   A Greasemonkey script that synchronizes your portfolio data on Nordnet to LulzCapita 
// @include       https://www.nordnet.fi/mux/web/user/overview.html
// ==/UserScript==

var portfolioId = document.evaluate("//div/@portfolio", document, null, XPathResult.ANY_TYPE, null).iterateNext().textContent;

var muut = document.evaluate("//div[div/@class='subBar']", document, null, XPathResult.ANY_TYPE, null).iterateNext();

// Error reporter
function check(msg,callback) {
    "use strict";
    return function(result) {
	if (callback !== null && result.status === 200) {
	    // Everything good.
	    callback(result.responseText);
	} else {
	    // Report issues.
	    var e = msg + ": " + result.statusText;
	    console.log(e);
	    alert(e);
	}
    };
}

function goodSync(a) {
    "use strict";
    alert("Salkku synkronoitu LulzCapitaan onnistuneesti!");
}

// Sends provided portfolio string to LulzCapita
function sendToLulz(portfolio) {
    "use strict";    
    GM_xmlhttpRequest({
	method: "POST",
	url: "http://capita.lulz.fi/bin/portfolio",
	data: portfolio,
	headers: {
	    "Content-Type": "text/plain; charset=UTF-8",
	    "PortfolioFormat": "nordnet",
	    "PortfolioID": portfolioId
	},
	onload: check("Tapahtumien lähettäminen tietokantaan epäonnistui",goodSync),
	onerror: check("Tapahtumien lähettäminen tietokantaan epäonnistui")
    });
}

// Fetches data from Nordnet and sends it to the provided callback.
function readAndSync() {
    "use strict";
    GM_xmlhttpRequest({
	method: "GET",
	url: "/mux/laddaner/transaktionsfil.html?year=all&month=all&trtyp=all&vp=all&curr=all&sorteringsordning=fallande&sortera=datum&startperiod=01-01-2000&endperiod=01-01-2020",
	overrideMimeType: "text/plain; charset=ISO-8859-1",
	onload: check("Tapahtumien lukeminen epäonnistui",sendToLulz),
	onerror: check("Tapahtumien lukeminen epäonnistui")
    });
}

// Adding a link and a listener on main page (in Finnish).
var div = document.createElement("div");
div.setAttribute("class", "section");
var href = document.createElement("a");
href.setAttribute("style", "cursor:pointer;");
href.textContent = "Synkronoi salkkusi LulzCapitaan";
href.addEventListener("click", readAndSync, false);
div.appendChild(href);
muut.appendChild(div);
