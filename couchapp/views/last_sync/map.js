// Finds the date of update (and reduce gives the last one)
function(doc) {
    if (doc.table != "sync") return;

    // Makes "Nordnet workaround" when converting timestamp. That is
    // we assume every transaction of previous day has been recorded
    // 12:00 UTC next day.
    var date = new Date(1000*(doc.time-43200));
    var iso8601 = date.toLocaleFormat("%Y-%m-%d");
    emit(doc.portfolio,iso8601);
}
