// FIXME to use sync table
// Finds the date of update (and reduce gives the last one)
function(doc) {
    emit(doc.portfolio,doc.date)
}
