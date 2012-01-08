// Used to find current number of stocks per user.
function(doc) {
    if (doc.table !== "portfolio") return;
    if (doc.type !== "sale") return;
    emit([doc.user,doc.isin],doc.count)
}
