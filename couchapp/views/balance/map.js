// When level=2: balance per stock (how much money in vs. money out)
// When level=1: balance on account
function(doc) {
    if (doc.table != "portfolio") return;
    emit([doc.user,doc.isin],doc.sum)
}
