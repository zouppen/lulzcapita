// Used to find current number of stocks per user.
function(doc) {
    if (doc.type != "sale") return;
    emit([doc.portfolio,doc.isin],doc.count)
}
