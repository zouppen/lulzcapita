// Useful in finding out what stocks there are or who has certain
// stocks. Reverse of user_stocks.
function(doc) {
    if (doc.table !== "portfolio") return;
    if (doc.type !== "sale") return;
    emit([doc.isin,doc.user],doc.count)
}
