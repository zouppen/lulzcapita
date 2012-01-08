// Looks for unknown transaction types
// This view is for humans, not machines.
function(doc) {
    if ( doc.type !== 'unknown' ) return;
    emit(null,null);
}
