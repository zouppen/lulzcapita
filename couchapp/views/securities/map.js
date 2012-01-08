// Shows information about securities. Strips "isin_" from
// the beginning of a document name (containing ISIN code).
// This view is for humans, not machines.
function(doc) {
    if ( doc.table !== 'security' ) return;
    emit(doc._id.substr(5),doc.raw.symbol);
}
