// ==UserScript==
// @name          LulzCapita for Nordnet
// @namespace     http://zouppen.iki.fi/2011/nordnet
// @description   A Greasemonkey script that synchronizes your portfolio data on Nordnet to LulzCapita 
// @include       https://www.nordnet.fi/mux/web/user/overview.html
// ==/UserScript==

var portfolioId = document.evaluate("//div/@portfolio", document, null, XPathResult.ANY_TYPE, null).iterateNext().textContent;

var muut = document.evaluate("//div[div/@class='subBar']", document, null, XPathResult.ANY_TYPE, null).iterateNext();

// Preparing elements on the main page.
var synced = false;
var start_date; // Filled in user request.
var div = document.createElement("div");
var last = document.createElement("span");
div.setAttribute("class", "section");

// Our own wrapper to Greasemonkey HTTP request.
function lulzHttpRequest(lulz) {
    "use strict";

    // Error message reporter.
    function fail(result) {
	var e = lulz.error + ": " + result.statusText;
	console.log(e);
	alert(e);
    }

    // Actual HTTP request.
    GM_xmlhttpRequest({
	method: lulz.method,
	url: (lulz.no_lulz ? "" : "http://capita.lulz.fi/bin/") + lulz.url,
	data: lulz.data,
	headers: lulz.no_lulz ? {} : {
	    "Content-Type": "text/plain; charset=UTF-8",
	    "PortfolioFormat": "nordnet",
	    "PortfolioID": portfolioId
	},
	overrideMimeType: lulz.overrideMimeType,
	onload: function(result) {
	    // Callback, if returns OK, otherwise error.
	    if (result.status === 200) {
		lulz.callback(result.responseText);
	    } else {
		fail(result);
	    }
	},
	onerror: fail
    });
}

function goodSync() {
    "use strict";
    alert("Salkku synkronoitu LulzCapitaan onnistuneesti!");
    last.textContent = "(juuri äsken synkronoitu)";
    synced = true;
}

// Fetches data from Nordnet and sends it to the provided callback.
function readAndSync() {
    "use strict";
    if (synced === true) {
	alert("Salkkusi on jo synkronoitu!");
	return;
    }
    
    lulzHttpRequest({
	no_lulz: true,
	method: "GET",
	url: "/mux/laddaner/transaktionsfil.html?year=all&month=all&trtyp=all&vp=all&curr=all&sorteringsordning=fallande&sortera=datum&startperiod="+start_date+"&endperiod=01-01-2020",
	overrideMimeType: "text/plain; charset=ISO-8859-1",
	error: "Tapahtumien lukeminen epäonnistui",
	callback: function(portfolio) { lulzHttpRequest({
	    method: "POST",
	    url: "portfolio",
	    data: portfolio,
	    callback: goodSync,
	    error: "Tapahtumien lähettäminen tietokantaan epäonnistui"
	});}
    });
}

// Getting user information asynchronously from LulzCapita.
lulzHttpRequest({
    method: "GET",
    url: "userinfo",
    error: "Käyttäjätietojen lataus LulzCapitasta epäonnistui",
    callback: function(json) {
	"use strict";
	var o = JSON.parse(json);

	// Making "Nordnet workaround" when converting timestamp. That
	// is we assume transactions on previous day have been logged
	// till 12:00 UTC next day.
	var trueDate = new Date(1000*(o.last));
	var nonDate = new Date(1000*(o.last-43200+(60*trueDate.getTimezoneOffset())));
	var finnish = trueDate.toLocaleFormat("%d.%m.%Y %H:%M");
	start_date = nonDate.toLocaleFormat("%d-%m-%Y");

	if (o.user === null) {
	    // No such user.
	    last.textContent =
		"Salkkuasi ei ole vielä rekisteröity LulzCapitaan. "+
		"Pyydä käyttöoikeutta Zouppenilta.";
	} else {
	    // User found.
	    var href = document.createElement("a");
	    href.setAttribute("style", "cursor:pointer;");
	    href.textContent = "Synkronoi salkkusi LulzCapitaan tunnuksella "+
		o.user.nick;
	    href.addEventListener("click", readAndSync, false);
	    last.textContent = o.is_first ?
		"(ei vielä synkronoitu)" : "(synkronoitu "+finnish+")";
	    div.appendChild(href);
	}
	div.appendChild(last);
	muut.appendChild(div);
    }
});
