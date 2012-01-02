// Finds the date of update (and reduce gives the last one)
function(doc) {
    if (doc.table != "sync") return;
    emit(doc.portfolio,doc.time);
}
