// Useful in finding out what stocks there are or who has certain
// stocks. Reverse of stockcount.
function(doc) {
    if (doc.type != "sale") return;
    emit([doc.isin,doc.portfolio],doc.count)
}
